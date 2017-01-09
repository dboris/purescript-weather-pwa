# purescript-weather-pwa

Google's [Your First Progressive Web App](https://developers.google.com/web/fundamentals/getting-started/codelabs/your-first-pwapp/) codelab translated to PureScript.

This Weather application has a relatively small code size and is a good place to
start learning both PureScript and Progressive Web App concepts. It can serve as
an example of how to use PureScript for:

* DOM manipulation
* AJAX requests
* JSON parsing
* Persisting data in local storage
* Caching files in cache storage
* Using service workers

...

## Installation

```
bower install
npm install
pulp build --main Main --to static/main.js
pulp build --main Worker --to static/worker.js
open static/index.html
```

## TODO

- [x] Implement your App Shell
- [x] Start with a fast first load
- [x] Use service workers to pre-cache the App Shell
- [ ] Use service workers to cache the forecast data
- [ ] Support native integration
