import asyncio
import zmq
import websockets

async def bridge(websocket):
    ctx = zmq.Context()
    socket = ctx.socket(zmq.SUB)
    socket.connect('ipc:///tmp/tabula')
    socket.setsockopt(zmq.CONFLATE, 1)
    socket.subscribe('')

    print("[INFO] Inited zmq...")

    while True:
        message = socket.recv()
        print(message)
        await websocket.send(message)

async def main():
    try:
        async with websockets.serve(bridge, host='0.0.0.0', port=6969, max_size=999999999):
            await asyncio.Future()
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    loop = asyncio.get_event_loop()
    loop.run_until_complete(asyncio.gather(main()))
