"""Render the 1-neighborhood of the styleme foreign key constraints
with clickable nodes to navigate around.
"""
import os
import sqlalchemy
import networkx as nx
import subprocess

def ensure_dir(d):
    try:
        os.mkdir(d)
    except os.error:
        pass


def render():
    e = sqlalchemy.create_engine('postgresql://127.0.0.1/holborn')
    i = sqlalchemy.inspect(e)

    fkg = nx.DiGraph()

    for name in i.get_table_names():
        for fk in i.get_foreign_keys(name):
            fkg.add_node(name, URL='"{}.svg"'.format(name), shape='rect')
            fkg.add_node(fk['referred_table'], URL='"{}.svg"'.format(fk['referred_table']), path=fk['referred_table'], shape='rect')
            fkg.add_edge(fk['referred_table'], name, arrowhead='dot', color='gray')
            fkg.add_edge(name, fk['referred_table'])

    for node in fkg.nodes_iter():
        sg = fkg.subgraph(nx.single_source_shortest_path_length(fkg, node, 1).keys() + [node])
        sg.graph['graph'] = {'rankdir': 'LR', 'ratio': 'compress'}

        path = 'tmp/{}.dot'.format(node)
        out_path = 'db-graph/{}.svg'.format(node)
        nx.write_dot(sg, path)
        subprocess.check_output(['dot', '-Tsvg', path, '-o',  out_path])


if __name__ == '__main__':
    ensure_dir('./tmp')
    ensure_dir('./db-graph')
    render()
    print "rendering done, go file://{}".format(os.path.abspath('db-graph/user.svg'))
