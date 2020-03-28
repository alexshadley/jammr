from flask import Flask, jsonify
from flask_socketio import SocketIO, emit

import json

app = Flask(__name__)
app.config['SECRET_KEY'] = 'secret!'
socketio = SocketIO(app, cors_allowed_origins='*')

notes = {}

@socketio.on('add_note_to_server')
def test_message(message):
    notes[message['id']] = {'id': message['id'], 'pitch': message['pitch'], 'start': message['start'], 'duration': message['duration']}
    emit('add_note_to_client', notes[message['id']], broadcast=True)

    print('note added')

@socketio.on('remove_note_from_server')
def test_message(message):
    del notes[message['id']]
    emit('remove_note_from_client', {'id': message['id']}, broadcast=True)

    print('note removed')

@socketio.on('connect')
def test_connect():
    print('Client connected')

    emit('set_notes', {'notes': [note for _, note in notes.items()]})

@socketio.on('disconnect')
def test_disconnect():
    print('Client disconnected')

if __name__ == '__main__':
    socketio.run(app)