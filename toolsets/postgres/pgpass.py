import argparse
import collections
import os
import re
import sys

ConnectionLine = collections.namedtuple('ConnectionLine', ['host', 'port', 'database', 'user', 'password'])

def build_alias_map(filepath=None):
    '''
    Load a file with the format:
    |  # alias:{alias}
    |  host:port:database:user:password
    |  ...
    into a map like {'alias': ConnectionLine(host='host', port='port', database='database', user='user'), ...}
    '''
    filepath = filepath or os.environ.get('PGPASSFILE', os.environ.get('HOME') + '/.pgpass')
    alias_comment_pattern = re.compile(r'# alias:(?P<alias>[^\s]+)\s*')
    alias_map = {}
    unaliased_connections = []

    with open(filepath) as f:
        alias = None
        for line in f:
            m = alias_comment_pattern.match(line)
            if m:
                alias = m.group('alias')
                continue
            else:
                fields = line.strip().split(':')
                if len(fields) == 5:
                    connection_line = ConnectionLine(*fields)
                    if alias:
                        alias_map[alias] = connection_line
                    else:
                        unaliased_connections.append(connection_line)

            alias = None

    unaliased_map = {c.database: c for c in reversed(unaliased_connections)}

    merged_map = {}
    merged_map.update(unaliased_map)
    merged_map.update(alias_map)
    return merged_map

def list_databases(alias_map):
    aliases = sorted(alias_map.keys())
    for alias in aliases:
        connection_line = alias_map[alias]
        print('{alias} -> {user}:***@{host}:{port}/{database}'.format(
            alias=alias,
            user=connection_line.user,
            host=connection_line.host,
            port=connection_line.port,
            database=connection_line.database,
            ))

def get_psql(alias_map, alias):
    connection_line = alias_map.get(alias)
    if not connection_line:
        raise KeyError("No connection found for alias: {}".format(alias))

    return 'psql -h {host} -p {port} -U {user} {database}'.format(
        user=connection_line.user,
        host=connection_line.host,
        port=connection_line.port,
        database=connection_line.database,
        )

def dump_psql(alias_map):
    aliases = sorted(alias_map.keys())
    for alias in aliases:
        print('--- ' + alias + ' ---');
        t = get_psql(alias_map, alias)
        print(t)

def get_datagrip(alias_map, alias):
    config_template = "user: {user}\npwd: {password}\nurl: {connection_string}"
    connection_template = 'jdbc:{type}://{host}:{port}/{database}'

    connection_line = alias_map[alias]
    dbext_connection_string = connection_template.format(
        type='postgresql',
        user=connection_line.user,
        password=connection_line.password,
        host=connection_line.host,
        port=connection_line.port,
        database=connection_line.database,
    )
    config_line = config_template.format(
        profile_name=alias.lower().replace('-', '_'),
        connection_string=dbext_connection_string,
        user=connection_line.user,
        password=connection_line.password,
    )
    return config_line

def dump_datagrip(alias_map):
    aliases = sorted(alias_map.keys())
    for alias in aliases:
        print('--- ' + alias + ' ---');
        t = get_datagrip(alias_map, alias)
        print(t)

def get_jdbc(alias_map, alias):
    config_template = "{connection_string}"
    connection_template = 'jdbc:{type}://{host}:{port}/{database}?user={user}&password={password}'

    connection_line = alias_map[alias]
    dbext_connection_string = connection_template.format(
        type='postgresql',
        user=connection_line.user,
        password=connection_line.password,
        host=connection_line.host,
        port=connection_line.port,
        database=connection_line.database,
    )
    config_line = config_template.format(
        profile_name=alias.lower().replace('-', '_'),
        connection_string=dbext_connection_string,
        user=connection_line.user,
        password=connection_line.password,
    )
    return config_line

def dump_jdbc(alias_map):
    aliases = sorted(alias_map.keys())
    for alias in aliases:
        print('--- ' + alias + ' ---');
        t = get_jdbc(alias_map, alias)
        print(t)

def parse_opts(argv=None):
    argv = argv or sys.argv[1:]

    parser = argparse.ArgumentParser(prog="postgres_pass", description="Manage psql connections")
    parser.set_defaults(func=lambda x: parser.print_help())

    subparsers = parser.add_subparsers()

    alias_map = build_alias_map()

    # pgpass.py list
    def cmd_list_databases(opts):
        list_databases(alias_map)
    list_cmd = subparsers.add_parser('list')
    list_cmd.set_defaults(func=cmd_list_databases)

    # pgpass.py print-psql {alias}
    def cmd_print_connection_command(opts):
        print(get_psql(alias_map, opts.alias[0]))
    connect_cmd = subparsers.add_parser('print-psql')
    connect_cmd.add_argument('alias', nargs=1)
    connect_cmd.set_defaults(func=cmd_print_connection_command)

    # pgpass.py dump-psql
    def cmd_print_connection_command(opts):
        dump_psql(alias_map)
    connect_cmd = subparsers.add_parser('dump-psql')
    connect_cmd.set_defaults(func=cmd_print_connection_command)

    # pgpass.py print-datagrip {alias}
    def cmd_print_connection_command(opts):
        print(get_datagrip(alias_map, opts.alias[0]))
    connect_cmd = subparsers.add_parser('print-datagrip')
    connect_cmd.add_argument('alias', nargs=1)
    connect_cmd.set_defaults(func=cmd_print_connection_command)

    # pgpass.py dump-datagrip
    def cmd_print_connection_command(opts):
        dump_datagrip(alias_map)
    connect_cmd = subparsers.add_parser('dump-datagrip')
    connect_cmd.set_defaults(func=cmd_print_connection_command)

    # pgpass.py print-jdbc {alias}
    def cmd_print_connection_command(opts):
        print(get_jdbc(alias_map, opts.alias[0]))
    connect_cmd = subparsers.add_parser('print-jdbc')
    connect_cmd.add_argument('alias', nargs=1)
    connect_cmd.set_defaults(func=cmd_print_connection_command)

    # pgpass.py dump-jdbc
    def cmd_print_connection_command(opts):
        dump_jdbc(alias_map)
    connect_cmd = subparsers.add_parser('dump-jdbc')
    connect_cmd.set_defaults(func=cmd_print_connection_command)

    return parser.parse_args(argv)

def main():
    opts = parse_opts()
    if opts:
        opts.func(opts)

if __name__ == '__main__':
    main()
