'use strict';

require("./styles.scss");

const {Elm} = require('./Main');
var app = Elm.Main.init({
    flags: {
      configBackendEndpoint: process.env.BACKEND_ENDPOINT,
	  configFrontendEndpoint: process.env.FRONTEND_ENDPOINT
    }
});