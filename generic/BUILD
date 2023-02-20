load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
    name = "lib",
    srcs = glob(["src/main/scala/**/*.scala"]),
    deps = [
        "//core:core-lib",
        "@third_party//3rdparty/jvm/com/chuusai:shapeless",
    ],
    exports = [
        "//core:core-lib",
        "@third_party//3rdparty/jvm/com/chuusai:shapeless",
    ],
    visibility = ["//visibility:public"],
)

assemble_maven(
    name = "assemble",
    target = ":lib",
    project_name = "hcl4s generic",
    project_description = "Generic Codec Derivation for hcl4s",
    scm_url = "https://github.com/jtrim777-dev/hcl4s",
    version_file = "//generic:VERSION",
    license = "mit",
    workspace_refs = "@repo_workspace_refs//:refs.json"
)

deploy_maven(
    name = "deploy",
    target = ":assemble",
    release = "https://maven.jtrim777.dev/releases",
    snapshot = "https://maven.jtrim777.dev/releases",
)