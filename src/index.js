'use strict';

require("./styles.scss");
// TEST
console.log("TEST");
console.log(process.env.FRONTEND_ENDPOINT);
const {Elm} = require('./Main');
var app = Elm.Main.init({
  flags: {
    configBackendEndpoint: process.env.BACKEND_ENDPOINT,
    configFrontendEndpoint: process.env.FRONTEND_ENDPOINT
  }
});