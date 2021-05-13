import firebase from 'firebase/app';
import 'firebase/database';
import 'firebase/auth';
import './app.scss';
import { Elm } from './Main.elm';

const firebaseConfig = {
    apiKey: "AIzaSyBB6mJ8zC4HeqpnznHVnrqvu7vq3i05HQU",
    authDomain: "tictactoe-7d2d5.firebaseapp.com",
    projectId: "tictactoe-7d2d5",
    storageBucket: "tictactoe-7d2d5.appspot.com",
    messagingSenderId: "1060266018339",
    appId: "1:1060266018339:web:c3a29cb4f0361ab8435396",
    databaseURL: 'https://tictactoe-7d2d5-default-rtdb.europe-west1.firebasedatabase.app/',
};

firebase.initializeApp(firebaseConfig);

const app = Elm.Main.init({
    node: document.querySelector('#app'),
});

let deregisterActiveGame;

app.ports.fbLogin.subscribe(async () => {
    console.log('Logging in Firebase..');

    try {
        const { user } = await firebase.auth().signInAnonymously();
        console.log({ user });

        app.ports.firebaseInput.send(['profile', user]);
    } catch (e) {
        console.error(e);
    }
});

app.ports.fbSelectActiveGame.subscribe((gameId) => {
    if (deregisterActiveGame) {
        deregisterActiveGame();
        deregisterActiveGame = null;
    }

    const callback = (snapshot) => {
        const game = snapshot.val();
        console.log({ game });
        app.ports.firebaseInput.send(['game', game]);
    };

    firebase.database().ref(`games/${gameId}`).on('value', callback);

    deregisterActiveGame = function () {
        firebase.database().ref(`games/${gameId}`).off('value', callback);
    };
});

app.ports.fbUpdateCells.subscribe(async ({ gameId, cells, activePlayer, winner }) => {
    console.log({ gameId, cells, activePlayer, winner });
    try {
        if (winner === 'None') {
            await firebase.database().ref(`games/${gameId}`).update({ activePlayer, cells });
        } else {
            await firebase.database().ref(`games/${gameId}`).update({ activePlayer, cells, winner });
        }
    } catch (e) {
        console.error(e);
    }
});


firebase.database().ref('gameList').on('value', (snapshot) => {
    const gameList = snapshot.val();
    console.log({ gameList });
    app.ports.firebaseInput.send(['gameList', gameList]);
});
