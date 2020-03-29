import * as Tone from 'tone';
import { Elm } from './Main.elm';
import io from 'socket.io-client';

var voices = [
    {oscillator: {type: 'pulse'}},
    {oscillator: {type: 'triangle'}},
    {oscillator: {type: 'square'}}
]

var synths = voices.map(v => new Tone.PolySynth(8, Tone.Synth, v).toMaster());

/*
ordinarily maintaining state in Elm and JS is a very bad thing to do. I've
broken the sacred rule here because the browser needs to tell the backend when
the user navigates away, and there's currently no way to do this in Elm.
*/
var user = null;


// socket.io connection to server
var socket = io.connect('http://localhost:5000');
socket.on('connect', function() {
});

window.addEventListener('beforeunload', (event) => {
    if (user !== null) {
        socket.emit('remove_user', user)
    }
});

socket.on('set_notes', (message) => {
    console.log(message)
    app.ports.setNotes.send(message['notes']);
});

socket.on('add_note_to_client', (message) => {
    console.log(message)
    app.ports.addNoteFromServer.send(message);
});

socket.on('remove_note_from_client', (message) => {
    console.log(message)
    app.ports.removeNoteFromServer.send(message);
});

socket.on('set_users', (message) => {
    console.log(message)
    app.ports.setUsersFromServer.send(message);
});

socket.on('user_registered', (message) => {
    console.log(message)
    user = message['name']
    app.ports.userRegisteredFromServer.send(message);
});

const app = Elm.Main.init({
    node: document.querySelector('main')
})

app.ports.addNote.subscribe(note => {
    socket.emit('add_note_to_server', note)
})

app.ports.removeNote.subscribe(note => {
    socket.emit('remove_note_from_server', note)
})

app.ports.addUser.subscribe(user => {
    socket.emit('add_user', user)
})

app.ports.playNotes.subscribe(noteInstructions => {
    noteInstructions.map(inst => playNote(inst['frequency'], inst['start'], inst['duration'], inst['voice']));
})

function playNote(freq, start, duration, voice) {
    synths[voice].triggerAttackRelease(freq, duration, '+' + start)
}


