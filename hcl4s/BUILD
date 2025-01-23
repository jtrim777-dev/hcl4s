# load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
  name = "full-lib",
  srcs = glob(["src/main/scala/*.scala"]),
  exports = [
    "//core:core-lib",
    "//parser:lib",
    "//generic:lib",
  ],
  deps = [
    "//core:core-lib",
    "//parser:lib",
    "//generic:lib",
  ],
  visibility = ["//visibility:public"],
  tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s_2.13:{pom_version}"],
)

# assemble_maven(
#     name = "assemble",
#     target = ":full-lib",
#     project_name = "hcl4s",
#     project_description = "Scala parser and interpreter for HCL",
#     scm_url = "https://github.com/jtrim777-dev/hcl4s",
#     version_file = "//:VERSION",
#     license = "mit",
#     workspace_refs = "@repo_workspace_refs//:refs.json"
# )
#
# deploy_maven(
#     name = "deploy",
#     target = ":assemble",
#     release = "https://maven.jtrim777.dev/releases",
#     snapshot = "https://maven.jtrim777.dev/releases",
# )

scala_binary(
    name = "bigtest",
    srcs = ["src/test/scala/BigTest.scala"],
    main_class = "dev.jtrim777.hcl4s.BigTest",
    deps = [
        ":full-lib",
        "@maven//:com_lihaoyi_pprint_2_13",
    ],
)

scala_binary(
    name = "rtest",
    srcs = ["src/test/scala/ResumeTest.scala"],
    main_class = "dev.jtrim777.hcl4s.ResumeTest",
    deps = [
        ":full-lib",
        "//extensions/json:lib",
        "@maven//:com_lihaoyi_pprint_2_13",
    ],
)

scala_binary(
    name = "repl",
    srcs = [],
    main_class = "ammonite.Main",
    deps = [
        ":full-lib",
        "@maven//:com_lihaoyi_ammonite_2_13_8",
    ],
)