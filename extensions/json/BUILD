load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
    name = "lib",
    srcs = glob(["src/main/scala/*.scala"]),
    visibility = ["//visibility:public"],
    exports = [
        "//core:lang-lib",
        "//core:util-lib",
        "//core:model-lib",
        "@ext//io/circe:circe_core",
        "@ext//io/circe:circe_generic"
    ],
    deps = [
        "//core:lang-lib",
        "//core:util-lib",
        "//core:model-lib",
        "@ext//io/circe:circe_core",
        "@ext//io/circe:circe_generic"
    ],
    tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s-json_2.13:{pom_version}"],
)

assemble_maven(
    name = "assemble",
    target = ":lib",
    project_name = "hcl4s JSON compat",
    project_description = "JSON codecs for hcl4s",
    scm_url = "https://github.com/jtrim777-dev/hcl4s",
    version_file = "//:VERSION",
    license = "mit",
    workspace_refs = "@repo_workspace_refs//:refs.json"
)

deploy_maven(
    name = "deploy",
    target = ":assemble",
    release = "https://maven.jtrim777.dev/releases",
    snapshot = "https://maven.jtrim777.dev/releases",
)
