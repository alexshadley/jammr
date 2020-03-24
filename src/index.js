import { Elm } from './Main.elm';

const context = new AudioContext()
const osc = context.createOscillator()
const amp = context.createGain()

osc.connect(amp)
amp.connect(context.destination)

amp.gain.value = 0
osc.start()

const app = Elm.Main.init({
    node: document.querySelector('main')
})

app.ports.play.subscribe(freq => {
    if (context.state !== 'running') {
        context.resume()
    }
    osc.frequency.value = freq
    amp.gain.linearRampToValueAtTime(0.1, context.currentTime + 0.05)
})

app.ports.stop.subscribe(() => {
    amp.gain.linearRampToValueAtTime(0.0, context.currentTime + 0.05)
})


