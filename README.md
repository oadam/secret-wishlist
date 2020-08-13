## build instructions
### setup

    apt install inotify-tools yarn
    yarn install

### dev

    make watch
    # in another terminal start the static webserver and navigate to localhost:8000/index.dev.html
    make serve

#### prod

    make prod
