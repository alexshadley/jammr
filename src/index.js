import * as Tone from 'tone';
import { Elm } from './Main.elm';

var synth = new Tone.PolySynth().toMaster();

const app = Elm.Main.init({
    node: document.querySelector('main')
})

app.ports.playNotes.subscribe(noteInstructions => {
    noteInstructions.map(inst => playNote(inst['frequency'], inst['start'], inst['duration']));
})

function playNote(freq, start, duration) {
    synth.triggerAttackRelease(freq, duration, '+' + start)
}


