# Orchestration CLI UI for Electron

## WIP
This is a work in progress and is not complete. It attempts to provide a UI for
constructing k8s GVK resources.

## Install
1. Clone this project
1. Load the `$PROJECT_ROOT/main.html` file in a browser.
   
## Build
1. Run `dev-env connect` in `$PROJECT_ROOT` (https://github.com/mrxk/dev-env)
1. In the container, run `make`

As an alternative to using `dev-env`, docker build
`$PROJECT_ROOT/.dev-env/main/Dockerfile` and use that container for the `make`
command.
