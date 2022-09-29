package io.github.vigoo.zioaws.codegen.loader

import software.amazon.awssdk.codegen.C2jModels
import zio.ZManaged.ReleaseMap
import zio._
import zio.blocking.Blocking
import zio.nio.core.file.Path
import zio.system._

import java.util.concurrent.atomic.AtomicReference

trait Loader {
  def findModels(): ZIO[Blocking, Throwable, Set[ModuleId]]
  def loadCodegenModel(id: ModuleId): ZIO[Blocking, Throwable, C2jModels]
}

object Loader {
  val fromClasspath: ULayer[Has[Loader]] = ZLayer.succeed(FromClasspath())
  val fromGit: ZLayer[System, Nothing, Has[Loader]] =
    {
      for {
        map <- Ref.make(Map.empty[ModuleId, Path])
        semaphore <- Semaphore.make(1)
        system <- ZIO.service[System.Service]
      } yield FromGit(map, semaphore, system)
    }.toLayer

  private val cachedLoader: AtomicReference[Loader] = new AtomicReference(null)
  def cached[R, E <: Throwable](
      impl: ZLayer[R, E, Has[Loader]]
  ): ZLayer[R, Throwable, Has[Loader]] =
    {
      for {
        runtime <- ZIO.runtime[R]
        value <- ZIO.effect {
          cachedLoader.updateAndGet { (cachedLoader: Loader) =>
            if (cachedLoader == null) {
                runtime.unsafeRun {
                  ReleaseMap.make.flatMap { releaseMap =>
                    impl
                      .build
                      .zio
                      .map(_._2.get)
                      .provideSome[R](_ -> releaseMap)
                  }
                }
            } else {
              cachedLoader
            }
          }
        }
      } yield value
    }.toLayer

  def loadCodegenModel(
      id: ModuleId
  ): ZIO[Has[Loader] with Blocking, Throwable, C2jModels] =
    ZIO.service[Loader].flatMap(_.loadCodegenModel(id))

  def findModels(): ZIO[Has[Loader] with Blocking, Throwable, Set[ModuleId]] =
    ZIO.service[Loader].flatMap(_.findModels())
}
