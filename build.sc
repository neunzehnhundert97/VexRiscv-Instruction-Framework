import mill._, scalalib._

object VexRiscvFramework extends ScalaModule {
  def scalaVersion = "2.12.13"

  def scalacOptions = Seq(
    "-feature",
    "-deprecation",
    "-explaintypes",
    "-Xlint",
    //"-Xfatal-warnings",
    // Linting options (taken from https://alexn.org/blog/2020/05/26/scala-fatal-warnings.html)
    "-unchecked",
    "-Xcheckinit",
    "-Xlint:adapted-args",
    "-Xlint:constant",
    "-Xlint:delayedinit-select",
    "-Xlint:deprecation",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow"
  )

  // Compiler plugins
  def scalacPluginIvyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.4.3"
      // Exclude transitive depedencies which somehow irritate metals
      .exclude("*" -> "*")
  )

  // Libraries
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.4.3",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.4.3",
    ivy"com.github.spinalhdl::vexriscv:2.1.0",
    ivy"com.github.pathikrit::better-files::3.9.1"
  )
}
