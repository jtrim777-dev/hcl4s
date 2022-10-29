# WORKSPACE
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

skylib_version = "1.0.3"
http_archive(
    name = "bazel_skylib",
    sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c",
    type = "tar.gz",
    url = "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/{}/bazel-skylib-{}.tar.gz".format(skylib_version, skylib_version),
)

rules_scala_version = "5df8033f752be64fbe2cedfd1bdbad56e2033b15"
http_archive(
    name = "io_bazel_rules_scala",
    sha256 = "b7fa29db72408a972e6b6685d1bc17465b3108b620cb56d9b1700cf6f70f624a",
    strip_prefix = "rules_scala-%s" % rules_scala_version,
    type = "zip",
    url = "https://github.com/bazelbuild/rules_scala/archive/%s.zip" % rules_scala_version,
)

# Stores Scala version and other configuration
# 2.12 is a default version, other versions can be use by passing them explicitly:
# scala_config(scala_version = "2.11.12")
load("@io_bazel_rules_scala//:scala_config.bzl", "scala_config")
scala_config(scala_version = "2.13.8")

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

load("//3rdparty:workspace.bzl", "maven_dependencies")
maven_dependencies()

load("//3rdparty:target_file.bzl", "build_external_workspace")
build_external_workspace(name = "third_party")

register_toolchains("//tools:tools_scala")

# optional: setup ScalaTest toolchain and dependencies
load("@io_bazel_rules_scala//testing:scalatest.bzl", "scalatest_repositories", "scalatest_toolchain")
scalatest_repositories()
scalatest_toolchain()

load("//tools:bazeldist.bzl", "import_bazeldist")
import_bazeldist()

load("@bazeldist//maven:deps.bzl", "maven_artifacts_with_versions")
load("@rules_jvm_external//:defs.bzl", "maven_install")
maven_install(
    artifacts = maven_artifacts_with_versions,
    fetch_sources = True,
    repositories = [
        "https://repo1.maven.org/maven2",
    ],
    strict_visibility = True,
    version_conflict_policy = "pinned",
)

load("@bazeldist//common:rules.bzl", "workspace_refs")
workspace_refs( name = "repo_workspace_refs" )

load("@io_bazel_rules_kotlin//kotlin:kotlin.bzl", "kotlin_repositories", "kt_register_toolchains")
kotlin_repositories()
kt_register_toolchains()
