module Config exposing (endpoint)

import Url


endpoint : Url.Url -> String
endpoint url =
    if url.host == "localhost" then
        "http://localhost:3000/"

    else
        "https://portfolio-performance-api.herokuapp.com/"
