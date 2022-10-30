load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

assemble_maven(
    name = "assemble-core",
    target = "//hcl4s:core-lib",
    project_name = "hcl4s-core",
    project_description = "Scala models, DSL, and interpreter for HCL",
    scm_url = "https://github.com/jtrim777-dev/hcl4s",
    version_file = "//:VERSION",
    license = "mit",
    workspace_refs = "@repo_workspace_refs//:refs.json"
)

deploy_maven(
    name = "deploy-core",
    target = ":assemble-core",
    release = "https://maven.jtrim777.dev/releases",
    snapshot = "https://maven.jtrim777.dev/releases",
)

assemble_maven(
    name = "assemble-full",
    target = "//hcl4s:lib",
    project_name = "hcl4s",
    project_description = "Scala parser and interpreter for HCL",
    scm_url = "https://github.com/jtrim777-dev/hcl4s",
    version_file = "//:VERSION",
    license = "mit",
    workspace_refs = "@repo_workspace_refs//:refs.json"
)

deploy_maven(
    name = "deploy-full",
    target = ":assemble-full",
    release = "https://maven.jtrim777.dev/releases",
    snapshot = "https://maven.jtrim777.dev/releases",
)