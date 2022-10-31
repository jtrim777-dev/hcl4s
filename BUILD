load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
  name = "full-lib",
  srcs = [],
  exports = [
    "//core:core-lib",
    "//parser:lib",
    "//codec:lib",
  ],
  deps = [
    "//core:core-lib",
    "//parser:lib",
    "//codec:lib",
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