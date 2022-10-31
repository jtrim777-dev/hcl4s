load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
    name = "util-lib",
    srcs = glob(["src/main/scala/util/*.scala"]),
    deps = [],
    exports = [],
    visibility = ["//:__subpackages__"]
)

scala_library(
    name = "lang-lib",
    srcs = glob(["src/main/scala/lang/**/*.scala"]),
    deps = [":util-lib",],
    exports = [":util-lib",],
    visibility = ["//:__subpackages__"]
)

scala_library(
    name = "model-lib",
    srcs = glob(["src/main/scala/model/*.scala"]),
    deps = [
        "@third_party//3rdparty/jvm/org/apache/commons:commons_text",
    ],
    exports = [
        "@third_party//3rdparty/jvm/org/apache/commons:commons_text",
    ],
    visibility = ["//:__subpackages__"]
)

scala_library(
    name = "eval-lib",
    srcs = glob(["src/main/scala/eval/*.scala"]),
    deps = [
        ":lang-lib",
        ":model-lib",
        ":util-lib",
    ],
    exports = [
        ":lang-lib",
        ":model-lib",
        ":util-lib",
    ],
)

scala_library(
  name = "core-lib",
  srcs = [],
  exports = [
    ":lang-lib",
    ":model-lib",
    ":eval-lib",
  ],
  deps = [
    ":lang-lib",
    ":model-lib",
    ":eval-lib",
  ],
  visibility = ["//visibility:public"],
  tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s-core_2.13:{pom_version}"],
)

assemble_maven(
    name = "assemble",
    target = ":core-lib",
    project_name = "hcl4s-core",
    project_description = "Scala models, DSL, and interpreter for HCL",
    scm_url = "https://github.com/jtrim777-dev/hcl4s",
    version_file = "//core:VERSION",
    license = "mit",
    workspace_refs = "@repo_workspace_refs//:refs.json"
)

deploy_maven(
    name = "deploy",
    target = ":assemble",
    release = "https://maven.jtrim777.dev/releases",
    snapshot = "https://maven.jtrim777.dev/releases",
)

scala_binary(
    name = "repl",
    srcs = [],
    main_class = "ammonite.Main",
    deps = [
        ":core-lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:ammonite_2_13_8",
    ],
)
