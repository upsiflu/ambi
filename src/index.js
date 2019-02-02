import './assets/main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './assets/registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { auth: "flupsi" }
});
app.ports.outgoing.subscribe (outgoing)
setTimeout ( function () { app.ports.incoming.send (site) }, 1000)


var site = {
    site: {
        title: "sss",
        created: "tttttt",
        creator: "flupsi",
        curator: "flupsi",
        app: exampleApp(),
        public: "00000",
        draft: "00000",
        datacopy: []
    }
    
}

function outgoing (data) {
    app.ports.receiveType.send (deliverType (data));
}

var exampleApp = function () { return `
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
