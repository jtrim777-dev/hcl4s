# load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
    name = "lib",
    srcs = glob(["src/main/scala/*.scala"]),
    visibility = ["//visibility:public"],
    exports = [
        "//core:lang-lib",
        "//core:util-lib",
        "@maven//:com_lihaoyi_fastparse_2_13",
        "@maven//:org_typelevel_cats_effect_2_13",
    ],
    deps = [
        "//core:lang-lib",
        "//core:util-lib",
        "@maven//:com_lihaoyi_fastparse_2_13",
        "@maven//:org_typelevel_cats_effect_2_13",
    ],
    tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s-parser_2.13:{pom_version}"],
)

# assemble_maven(
#     name = "assemble",
#     target = ":lib",
#     project_name = "hcl4s-parser",
#     project_description = "Syntax parser and loader for HCL",
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
    name = "test",
    srcs = ["src/test/scala/TestParse.scala"],
    deps = [
        "//parser:lib",
        "@maven//:com_lihaoyi_pprint_2_13",
    ],
    main_class = "dev.jtrim777.hcl4s.parser.TestParse",
)

scala_binary(
    name = "repl",
    srcs = [],
    main_class = "ammonite.Main",
    deps = [
        ":lib",
        "@maven//:com_lihaoyi_ammonite_2_13_8",
    ],
)
