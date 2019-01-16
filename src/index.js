import './assets/main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './assets/registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});
//app.ports.requestType.subscribe (requestType)



function requestType (typeID) {
    app.ports.receiveType.send (deliverType (typeID));
}

function deliverType (typeID) {
    return `
    { "flupsicom":
        { "works + work + paragraph": {}
        , "CV":
            { "title": {}
            , "+ paragraph +":
                { "text": {}
                , "footnote":
                    { "representation" : {}
                    , "definition work" : {}
                    , "definition" :
                        { "title": {}
                        , "+":
                            { "description": {}
                            , "hyperlink": {}
                            }
                        }
                    }
                }
            }
        }
    }`
}


registerServiceWorker();
