import * as Tone from 'tone';
import { Elm } from './Main.elm';
import io from 'socket.io-client';

var synth = new Tone.PolySynth(8).toMaster();

// socket.io connection to server
var socket = io.connect('http://localhost:5000');
socket.on('connect', function() {
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

const app = Elm.Main.init({
    node: document.querySelector('main')
})

app.ports.addNote.subscribe(note => {
    socket.emit('add_note_to_server', note)
})

app.ports.removeNote.subscribe(note => {
    socket.emit('remove_note_from_server', note)
})

app.ports.playNotes.subscribe(noteInstructions => {
    noteInstructions.map(inst => playNote(inst['frequency'], inst['start'], inst['duration']));
})

function playNote(freq, start, duration) {
    synth.triggerAttackRelease(freq, duration, '+' + start)
}


