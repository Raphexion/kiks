import requests
import click
from pika import (BlockingConnection,
                  PlainCredentials,
                  ConnectionParameters)


@click.command()
@click.option('--host', default='127.0.0.1')
@click.option('--port', default='15672')
@click.option('--username', default='guest')
@click.option('--password', default='guest')
def main(host, port, username, password):
    url = f'http://{username}:{password}@{host}:15672/api/queues'
    resp = requests.get(url)
    queues = resp.json()

    credentials = PlainCredentials(username, password)
    parameters = ConnectionParameters(host,
                                      5672,
                                      '/',
                                      credentials)
    conn = BlockingConnection(parameters)
    channel = conn.channel()

    for queue in queues:
        name = queue['name']
        if name.startswith('amq.gen'):
            print(name)
            channel.queue_delete(queue=name)

    # import pdb
    # pdb.set_trace()

if __name__ == '__main__':
    main()
