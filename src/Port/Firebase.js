import firebase from 'firebase/app';
import 'firebase/database';
import 'firebase/auth';

export default (ports) => {
    function loop(action, state) { // We don't need this for application specific states. Maybe we need for subscription listeners
        return action.$$nextState || state;
    }
    let exitFn;

    let signalConsume;
    const queue = [];
    async function consumeQueue() {
        console.debug('[firebase:queue] WAITING FOR NEW ITEM..');
        const msgImmediate = queue.shift();
        if (msgImmediate) {
            return msgImmediate;
        }

        return new Promise(resolve => {
            signalConsume = function () {
                const msg = queue.shift();
                console.debug('[firebase:queue] ... ITEM SHIFTED OUT');
                resolve(msg);

                signalConsume = null;
            };
        })
    }

    ports.firebaseOutput.subscribe(taggedValue => {
        const { tag, value } = taggedValue;
        queue.push({ tag, value })
        console.debug('[firebase:port] RECEIVED', JSON.stringify({
            taggedValue,
            queue
        }));

        if (signalConsume) {
            console.debug('[firebase:port] Signaling...');
            signalConsume();
        } else {
            console.warn('[firebase:port] Producer is going fast...',
                JSON.stringify({ queue } , 2, true ));
        }
    });

    return async () => {
        let state = {
            channels: [],
            destroyers: [],
        };
        const initMsg = await consumeQueue();

        if (initMsg.tag === '$$call') {
            const { callee, params : [ value ] } = initMsg.value;
            if (callee === '$$init') {
                init(state, value);
                await ports.firebaseInput.send(['$$return', { callee: '$$init', value: true }]);
            }
        }

        let loopNumber = 0;
        while (true) {
            console.group(`[firebase:loop] ${++loopNumber}`, { queue });
            console.debug('[firebase:loop] START...');
            const nextMsg = await consumeQueue();
            console.debug('[firebase:loop]', { nextMsg });

            if (nextMsg.tag === '$$exit') {
                exitFn(0);
            } else if (nextMsg.tag === '$$call') {
                const { callee, params = [] } = nextMsg.value;
                if (!(callee in callbacks)) {
                    console.error(`Function ${callee} not found in callbacks`);
                    continue;
                }

                const fn = callbacks[callee];
                const action = await fn.apply(null, [{ ...state, ports }, ...(params || [])] );
                console.debug('[firebase:loop]', { action });

                const nextState = loop(action, state);
                console.debug('[firebase:loop]', {nextState});

                state = nextState;

                if (action.tag === '$$return') {
                    await ports.firebaseInput.send(['$$return', { callee, value: action.value }]);
                }
            }
            console.groupEnd();
        }

        return 0;
    };
}

function init(state, firebaseConfig) {
    firebase.initializeApp(firebaseConfig);
}

function tagReturn(value) {
    return { tag: '$$return', value };
}

function makePacket(channel, packet) {
    return ({ channel, packet });
}

function tagError(error) {
    return { tag: '$$error', value: error };
}

const callbacks = {
    async fbLogin(state) {
        console.log('Logging in Firebase..');

        let $$return;
        try {
            const {user} = await firebase.auth().signInAnonymously();
            console.log({user});

            // const gameListItem = await firebase.database().ref(`gameList/${user.uid}`).get();
            // if (!gameListItem.exists()) {
            //     await firebase.database().ref(`gameList/${user.uid}`).set({
            //         id: user.uid,
            //         title: `${user.uid.substring(0, 9)}'s game`,
            //     });
            // }
            // const game = await firebase.database().ref(`games/${user.uid}`).get();
            // TODO issue command $$cmd $create
            // if (!game.exists()) {
            //     await firebase.database().ref(`games/${user.uid}`).set({
            //         id: user.uid,
            //         cells: '---------',
            //         player1: user.uid,
            //         activePlayer: user.uid,
            //     });
            // }
            console.log(`Listening on players/${user.uid}/requests...`);
            // TODO issue command $$listen
            // firebase.database().ref(`players/${user.uid}/requests`).on('child_added', (snapshot) => {
            //     const requesterUid = snapshot.val();
            //     console.log('requesterUid', requesterUid);
            //     app.ports.firebaseInput.send(['requester', requesterUid]);
            // });

            $$return = tagReturn(user);
        } catch (e) {
            console.error(e);
            $$return = tagError(e.message);
        }

        return $$return;
    },
    async fbGetValueAt(state, firebasePath) {
        console.log(`fbGetValueAt ${firebasePath}...`);

        let $$return;
        try {
            const snapshot = await firebase.database().ref(firebasePath).get();
            if (snapshot.exists()) {
                $$return = tagReturn(snapshot.val());
            } else {
                $$return = tagReturn(null);
            }
        } catch (e) {
            console.error(e);
            $$return = tagError(e.message);
        }

        return $$return;
    },
    async fbUpdateValueAt(state, firebasePath, value) {
        console.log(`fbUpdateValueAt ${firebasePath}:`, value);

        let $$return;
        try {
            await firebase.database().ref(firebasePath).update(value);

            $$return = tagReturn(true);
        } catch (e) {
            console.error(e);
            $$return = tagError(e.message);
        }

        return $$return;
    },
    async fbMultiUpdate(state, updates) {
        console.log(`fbMultiUpdate:`, updates);

        if (!Object.keys(updates).length) {
            return tagReturn(false);
        }

        let $$return;
        try {
            await firebase.database().ref().update(updates);
            $$return = tagReturn(true);
        } catch (e) {
            console.error(e);
            $$return = tagError(e.message);
        }

        return $$return;
    },
    async fbPushValue(state, firebasePath, value) {
        console.log(`fbPushValue ${value} into ${firebasePath}...`);

        let $$return;

        try {
            await firebase.database().ref(firebasePath).push(value);
            $$return = tagReturn(true);
        } catch (e) {
            console.error(e);
            $$return = tagError(e.message);
        }

        return $$return;
    },
    async fbRemoveValueFromList(state, firebasePath, value) {
        console.log(`fbRemoveValueFromList ${value} into ${firebasePath}...`);

        let $$return;

        try {
            await firebase.database().ref(firebasePath).transaction((existingList) => {
                return Object.entries(existingList)
                    .filter(([_, existingValue]) => existingValue !== value)
                    .reduce((acc, [key, existingValue]) => {
                        acc[key] = existingValue;
                        return acc;
                    }, {});
            });

            $$return = tagReturn(true);
        } catch (e) {
            $$return = tagError(e.message);
        }

        return $$return;
    },
    async fbListenOn({ ports, ...state }, firebasePath, eventType) {
        const channel = `${firebasePath}:${eventType}`;
        console.log('[callbacks: fbListenOn]', channel);
        const callback = function (snapshot) {
            const value = snapshot.val();
            const packet = makePacket(channel, value);

            console.log('Listener got incoming value:', packet);
            ports.firebaseInput.send(['$$stream', packet]);
        };

        console.info(`Listening on ${firebasePath}:${eventType}...`);
        firebase.database().ref(firebasePath).on(eventType, callback);
        const destroyer = () => {
            firebase.database().ref(firebasePath).off(eventType, callback);
        };

        return {
            $$nextState: {
                channels: [ ...state.channels, channel ],
                destroyers: [ ...state.destroyers, destroyer ],
            },
            ...tagReturn(channel),
        };
    },
    async fbListenOff(state, channel) {
        console.log('[callbacks: fbListenOff]', channel);
        const index = state.channels.indexOf(channel);

        let nextState = { ...state };
        if (index !== -1) {
            nextState.channels = nextState.channels.filter((_, key) => key !== index);
            const destroyer = nextState.destroyers[index];
            if (typeof destroyer === 'function') {
                destroyer();
            }
            nextState.destroyers = nextState.destroyers.filter((_, key) => key !== index);

            console.info(`Removing Firebase listener at ${channel}`, { channel, destroyer });
        }

        return {
            $$nextState: nextState,
            ...tagReturn(true),
        };
    }
};
