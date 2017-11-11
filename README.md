# A-star visualisation

Allows you to visualise the progress of a-star calculation.

## Dev

Build and watch elm code:

    elm-app start

Build and watch elm css:

    npm i -g chokidar
    chokidar 'src/Styles.elm' -c 'elm-css src/Stylesheets.elm'

## Deployment

Uses github pages:

    elm-app build
    gh-pages -d build

## Extension ideas

- Save / share terrain in base64 url
- Control iteration speed / rendering rate
- Control grid size / number of squares
- Some way of visualising the costs again
- Other algos
- Racing algos
- Move start / goal around
