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

        const gameListItem = await firebase.database().ref(`gameList/${user.uid}`).get();
        if (!gameListItem.exists()) {
            await firebase.database().ref(`gameList/${user.uid}`).set({
                id: user.uid,
                title: `${user.uid.substring(0, 9)}'s game`,
            });
        }
        const game = await firebase.database().ref(`games/${user.uid}`).get();
        if (!game.exists()) {
            await firebase.database().ref(`games/${user.uid}`).set({
                id: user.uid,
                cells: '---------',
                player1: user.uid,
                activePlayer: user.uid,
            });
        }
        console.log(`Listening on players/${user.uid}/requests...`);
        firebase.database().ref(`players/${user.uid}/requests`).on('child_added', (snapshot) => {
            const requesterUid = snapshot.val();
            console.log('requesterUid', requesterUid);
            app.ports.firebaseInput.send(['requester', requesterUid]);
        });

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

app.ports.fbRequestToJoinGame.subscribe(async ({ gameId, player2 }) => {
    await firebase.database().ref(`players/${gameId}/requests`).push(player2);
});

app.ports.fbAllowJoinRequest.subscribe(async ({ gameId, player2 }) => { // Received when player1 allows player2 to join
    const updates = {
        [`players/${gameId}/requests`]: null,
        [`games/${gameId}/player2`]: player2,
    };
    await firebase.database().ref().update(updates);
});

app.ports.fbDenyJoinRequest.subscribe(async ({ gameId, player2 }) => { // Received when player1 allows player2 to join
    await Promise.all([
        firebase.database().ref(`players/${gameId}/requests`).transaction((requests) => {
            console.log({ requests });
            return Object.entries(requests)
                .filter(([_, value]) => value !== player2)
                .reduce((acc, [key, value]) => {
                    acc[key] = value;
                    return acc;
                }, {});
        }),
    ]);
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
    const value = snapshot.val();
    const gameList = Object.values(value);
    console.log({ gameList });
    app.ports.firebaseInput.send(['gameList', gameList]);
});
