import * as Tone from 'tone';
import { Elm } from './Main.elm';
import io from 'socket.io-client';

var voices = [
    /*{oscillator: {type: 'pulse'}},
    {oscillator: {type: 'triangle'}},
    {oscillator: {type: 'square'}}*/
    { 'A4': 'samples/piano/A4.mp3' }
]

var piano = {
    'A0': 'A0.[mp3|ogg]',
    'A1': 'A1.[mp3|ogg]',
    'A2': 'A2.[mp3|ogg]',
    'A3': 'A3.[mp3|ogg]',
    'A4': 'A4.[mp3|ogg]',
    'A5': 'A5.[mp3|ogg]',
    'A6': 'A6.[mp3|ogg]',
    'A#0': 'As0.[mp3|ogg]',
    'A#1': 'As1.[mp3|ogg]',
    'A#2': 'As2.[mp3|ogg]',
    'A#3': 'As3.[mp3|ogg]',
    'A#4': 'As4.[mp3|ogg]',
    'A#5': 'As5.[mp3|ogg]',
    'A#6': 'As6.[mp3|ogg]',
    'B0': 'B0.[mp3|ogg]',
    'B1': 'B1.[mp3|ogg]',
    'B2': 'B2.[mp3|ogg]',
    'B3': 'B3.[mp3|ogg]',
    'B4': 'B4.[mp3|ogg]',
    'B5': 'B5.[mp3|ogg]',
    'B6': 'B6.[mp3|ogg]',
    'C0': 'C0.[mp3|ogg]',
    'C1': 'C1.[mp3|ogg]',
    'C2': 'C2.[mp3|ogg]',
    'C3': 'C3.[mp3|ogg]',
    'C4': 'C4.[mp3|ogg]',
    'C5': 'C5.[mp3|ogg]',
    'C6': 'C6.[mp3|ogg]',
    'C7': 'C7.[mp3|ogg]',
    'C#0': 'Cs0.[mp3|ogg]',
    'C#1': 'Cs1.[mp3|ogg]',
    'C#2': 'Cs2.[mp3|ogg]',
    'C#3': 'Cs3.[mp3|ogg]',
    'C#4': 'Cs4.[mp3|ogg]',
    'C#5': 'Cs5.[mp3|ogg]',
    'C#6': 'Cs6.[mp3|ogg]',
    'D0': 'D0.[mp3|ogg]',
    'D1': 'D1.[mp3|ogg]',
    'D2': 'D2.[mp3|ogg]',
    'D3': 'D3.[mp3|ogg]',
    'D4': 'D4.[mp3|ogg]',
    'D5': 'D5.[mp3|ogg]',
    'D6': 'D6.[mp3|ogg]',
    'D#0': 'Ds0.[mp3|ogg]',
    'D#1': 'Ds1.[mp3|ogg]',
    'D#2': 'Ds2.[mp3|ogg]',
    'D#3': 'Ds3.[mp3|ogg]',
    'D#4': 'Ds4.[mp3|ogg]',
    'D#5': 'Ds5.[mp3|ogg]',
    'D#6': 'Ds6.[mp3|ogg]',
    'E0': 'E0.[mp3|ogg]',
    'E1': 'E1.[mp3|ogg]',
    'E2': 'E2.[mp3|ogg]',
    'E3': 'E3.[mp3|ogg]',
    'E4': 'E4.[mp3|ogg]',
    'E5': 'E5.[mp3|ogg]',
    'E6': 'E6.[mp3|ogg]',
    'F0': 'F0.[mp3|ogg]',
    'F1': 'F1.[mp3|ogg]',
    'F2': 'F2.[mp3|ogg]',
    'F3': 'F3.[mp3|ogg]',
    'F4': 'F4.[mp3|ogg]',
    'F5': 'F5.[mp3|ogg]',
    'F6': 'F6.[mp3|ogg]',
    'F#0': 'Fs0.[mp3|ogg]',
    'F#1': 'Fs1.[mp3|ogg]',
    'F#2': 'Fs2.[mp3|ogg]',
    'F#3': 'Fs3.[mp3|ogg]',
    'F#4': 'Fs4.[mp3|ogg]',
    'F#5': 'Fs5.[mp3|ogg]',
    'F#6': 'Fs6.[mp3|ogg]',
    'G0': 'G0.[mp3|ogg]',
    'G1': 'G1.[mp3|ogg]',
    'G2': 'G2.[mp3|ogg]',
    'G3': 'G3.[mp3|ogg]',
    'G4': 'G4.[mp3|ogg]',
    'G5': 'G5.[mp3|ogg]',
    'G6': 'G6.[mp3|ogg]',
    'G#0': 'Gs0.[mp3|ogg]',
    'G#1': 'Gs1.[mp3|ogg]',
    'G#2': 'Gs2.[mp3|ogg]',
    'G#3': 'Gs3.[mp3|ogg]',
    'G#4': 'Gs4.[mp3|ogg]',
    'G#5': 'Gs5.[mp3|ogg]',
    'G#6': 'Gs6.[mp3|ogg]'
}

var bass = {
    'A#2': 'As2.[mp3|ogg]',
    'A#3': 'As3.[mp3|ogg]',
    'A#4': 'As4.[mp3|ogg]',
    'A#5': 'As5.[mp3|ogg]',
    'C#2': 'Cs2.[mp3|ogg]',
    'C#3': 'Cs3.[mp3|ogg]',
    'C#4': 'Cs4.[mp3|ogg]',
    'C#5': 'Cs5.[mp3|ogg]',
    'E2': 'E2.[mp3|ogg]',
    'E3': 'E3.[mp3|ogg]',
    'E4': 'E4.[mp3|ogg]',
    'E5': 'E5.[mp3|ogg]',
    'G2': 'G2.[mp3|ogg]',
    'G3': 'G3.[mp3|ogg]',
    'G4': 'G4.[mp3|ogg]',
    'G5': 'G5.[mp3|ogg]'
}

var synths = []
synths[0] = new Tone.Sampler(piano, {baseUrl: 'samples/piano/'}).toMaster();
synths[1] = new Tone.Sampler(bass, {baseUrl: 'samples/bass/'}).toMaster();


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
    app.ports.setNotesFromServer.send(message);
});

socket.on('add_note', (message) => {
    console.log(message)
    app.ports.addNoteFromServer.send(message);
});

socket.on('update_notes', (message) => {
    console.log(message)
    app.ports.updateNotesFromServer.send(message);
});

socket.on('remove_note', (message) => {
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

console.log(app.ports);

app.ports.addNote.subscribe(note => {
    socket.emit('add_note', note)
})

app.ports.updateNotes.subscribe(notes => {
    socket.emit('update_notes', notes)
})

app.ports.removeNote.subscribe(note => {
    socket.emit('remove_note', note)
})

app.ports.addUser.subscribe(user => {
    socket.emit('add_user', user)
})

app.ports.stopPlayback.subscribe(() => {
    stopPlayback();
});

app.ports.playNotes.subscribe(noteInstructions => {
    stopPlayback();
    noteInstructions.map(inst => playNote(inst['pitch'], inst['start'], inst['duration'], inst['voice']));
    Tone.Transport.start();
})

function stopPlayback() {
    Tone.Transport.stop();
    Tone.Transport.cancel();
    synths.map(s => s.releaseAll());
}

function playNote(pitch, start, duration, voice) {
    // transport is used here because of a suspected bug in tonejs. For
    // whatever reason, triggerAttackRelease is broken for sampler
    Tone.Transport.schedule(function(_) {
        synths[voice].triggerAttack(pitch);
    }, start);
    Tone.Transport.schedule(function(_) {
        synths[voice].triggerRelease(pitch);
    }, start + duration);
}