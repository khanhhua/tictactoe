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
        console.log('fbSelectActiveGame:', { game });
        app.ports.firebaseInput.send(['game', game]);
    };

    firebase.database().ref(`games/${gameId}`).on('value', callback);

    deregisterActiveGame = function () {
        firebase.database().ref(`games/${gameId}`).off('value', callback);
    };
});

app.ports.fbJoinGame.subscribe(async ({ gameId, player2 }) => {
    if (deregisterActiveGame) {
        deregisterActiveGame();
        deregisterActiveGame = null;
    }

    const callback = (snapshot) => {
        const game = snapshot.val();
        console.log('fbJoinGame:', { game });
        app.ports.firebaseInput.send(['game', game]);
    };

    await firebase.database().ref(`games/${gameId}`).update({ player2 });

    firebase.database().ref(`games/${gameId}`).on('value', callback);
    deregisterActiveGame = function () {
        console.log(`Unsubscribing from games/${gameId}...`);
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

app.ports.fbStartNewGame.subscribe(async ({ gameId, cells, activePlayer, winner }) => {
    console.log({ gameId, cells, activePlayer, winner });
    try {
        await firebase.database().ref(`games/${gameId}`).update({ activePlayer, cells, winner });
    } catch (e) {
        console.error(e);
    }
});

app.ports.fbLeaveGame.subscribe(async ({ gameId, activePlayer }) => {
    if (deregisterActiveGame) {
        deregisterActiveGame();
        deregisterActiveGame = null;
    }

    try {
        await firebase.database().ref(`games/${gameId}`).update({
            activePlayer,
            player2: null,
        });
    } catch (e) {
        console.error(e);
    }
});


firebase.database().ref('gameList').on('value', (snapshot) => {
    const gameList = snapshot.val();
    console.log({ gameList });
    app.ports.firebaseInput.send(['gameList', gameList]);
});
