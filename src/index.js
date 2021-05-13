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

app.ports.fbLogin.subscribe(async () => {
    console.log('Logging in Firebase..');

    try {
        const { user } = await firebase.auth().signInAnonymously();
        console.log({ user });

        app.ports.fbProfile.send(user);
    } catch (e) {
        console.error(e);
    }
});

app.ports.fbUpdateCells.subscribe(async (cellString) => {
    console.log({ cellString });
    try {
        await firebase.database().ref('cells').set(cellString);
    } catch (e) {
        console.error(e);
    }
});

firebase.database().ref().on('value', (snapshot) => {
    const game = snapshot.val();
    console.log({ game });
    app.ports.fbGameUpdated.send(game);
});
