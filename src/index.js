import { Elm } from './Main.elm'
console.log("TEST : process.env.BACKEND_ENDPOINT");
console.log(process.env.BACKEND_ENDPOINT);
Elm.Main.init({
  node: document.querySelector('main')
})
