name := "algebra"
version := "1.0"
scalaVersion := "2.12.1"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.10.0"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"