import './app.scss';
import { Elm } from './Main.elm';

import FBinit from './Port/Firebase';

const app = Elm.Main.init({
    node: document.querySelector('#app'),
});

(FBinit(app.ports)()).then((exit) => console.log());

app.ports.bootstrap.subscribe(msg => {
    switch (msg) {
        case "modal.show":
            document.body.className = 'modal-open';
            break;
        case "modal.hide":
            document.body.className = '';
            break;
        default:
            break;
    }
});
