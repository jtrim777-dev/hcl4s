load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
  name = "full-lib",
  srcs = glob(["src/main/scala/*.scala"]),
  exports = [
    "//core:core-lib",
    "//parser:lib",
  ],
  deps = [
    "//core:core-lib",
    "//parser:lib",
  ],
  visibility = ["//visibility:public"],
  tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s_2.13:{pom_version}"],
)

assemble_maven(
    name = "assemble",
    target = ":full-lib",
    project_name = "hcl4s",
    project_description = "Scala parser and interpreter for HCL",
    scm_url = "https://github.com/jtrim777-dev/hcl4s",
    version_file = "//hcl4s:VERSION",
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
    name = "bigtest",
    srcs = ["src/test/scala/BigTest.scala"],
    main_class = "dev.jtrim777.hcl4s.BigTest",
    deps = [
        ":full-lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:pprint",
    ],
)

scala_binary(
    name = "repl",
    srcs = [],
    main_class = "ammonite.Main",
    deps = [
        ":full-lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:ammonite_2_13_8",
    ],
)