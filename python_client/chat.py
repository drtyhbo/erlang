import asyncore
import json
import socket
import sys
import threading
import Queue
import StringIO

HOST = '127.0.0.1'
PORT = 49165

def process_line(line):
    pieces = line.split(' ')
    if pieces[0] == 'connect':
        c.login(pieces[1], pieces[2])
    elif pieces[0] == 'send':
        c.send_message(pieces[1], ' '.join(pieces[2:]))

def add_input(input_queue):
    line = ''
    while True:
        character = sys.stdin.read(1)
        if character == '\n':
            process_line(line)
            line = ''
        else:
            line += character

class ChatClient(asyncore.dispatcher):
    def __init__(self):
        asyncore.dispatcher.__init__(self)

        self.to_write = ''
        self.read_buffer = StringIO.StringIO()

        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect((HOST, PORT))

    ## functions ##

    def login(self, username, session_token):
        self.__send_json({
            "c": username,
            "s": session_token
        })

    def send_message(self, to_username, message):
        self.__send_json({
            "r": to_username,
            "m": message
        })

    def __send_json(self, json_to_write):
        self.to_write += json.dumps(json_to_write)

    def process_json(self, json_to_process):
        if 'r' in json_to_process:
            print ">>> ok connected <<<"
        elif 's' in json_to_process and 'm' in json_to_process:
            print "%s: %s" % (json_to_process['s'], json_to_process['m'])

    ## asyncore.dispatcher delegate ##

    def handle_connect(self):
        print ("handle_connect()")

    def handle_close(self):
        print ("handle_close()")
        self.close()

    def writable(self):
        return True
    
    def readable(self):
        return True

    def handle_write(self):
        sent = self.send(self.to_write)
        self.to_write = self.to_write[sent:]

    def handle_read(self):
        data = self.recv(8192)
        self.process_json(json.loads(data))

c = ChatClient()

input_queue = Queue.Queue()

input_thread = threading.Thread(target=add_input, args=(input_queue,))
input_thread.daemon = True
input_thread.start()

asyncore.loop()