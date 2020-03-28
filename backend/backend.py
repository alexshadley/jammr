from flask import Flask, jsonify
from flask_socketio import SocketIO, emit

import json
import random

app = Flask(__name__)
app.config['SECRET_KEY'] = 'secret!'
socketio = SocketIO(app, cors_allowed_origins='*')

users = {}
notes = {}

@socketio.on('add_note_to_server')
def add_note(message):
    notes[message['id']] = {'id': message['id'], 'pitch': message['pitch'], 'start': message['start'], 'duration': message['duration']}
    emit('add_note_to_client', notes[message['id']], broadcast=True)

    print('note added')

@socketio.on('remove_note_from_server')
def remove_note(message):
    del notes[message['id']]
    emit('remove_note_from_client', {'id': message['id']}, broadcast=True)

    print('note removed')

@socketio.on('add_user')
def add_user(message):

    def transform_color(c):
        return 255 - abs((c % 512) - 255)

    base = random.randrange(512)
    color = [transform_color(c) for c in [base, base + 128, base + 256]]
    users[message['name']] = {'name': message['name'], 'color': color}

    emit('user_registered', users[message['name']])
    emit('set_users', [user for _, user in users.items()], broadcast=True)

    print(f'{message["name"]} added with color {color}')


@socketio.on('remove_user')
def remove_user(name):
    del users[name]
    emit('set_users', [user for _, user in users.items()], broadcast=True)

    print(f'{name} disconnected')
    

@socketio.on('connect')
def test_connect():
    print('Client connected')

    emit('set_notes', {'notes': [note for _, note in notes.items()]})

@socketio.on('disconnect')
def test_disconnect():
    print('Client disconnected')

if __name__ == '__main__':
    socketio.run(app)