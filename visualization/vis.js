var idctr = 0;

var make_graph = function (nodes, edges, lastnode) {
    return {
        nodes: nodes,
        edges: edges,
        lastnode: lastnode,
        add_node: function (type, labelstring) {
            idctr = idctr + 1;

            var node = {
                id: idctr,
                label: labelstring,
                group: type
            };

            return make_graph([node].concat(this.nodes), this.edges, node.id);
        },
        add_edge: function (src, dest) {
            var edge = { from: src.lastnode, to: dest.lastnode };

            return make_graph(this.nodes, [edge].concat(this.edges), this.lastnode);
        },
        set_node: function (node) {
            return make_graph(this.nodes, this.edges, node.lastnode);
        }
    }
}

var types = {
    uni: "uni",
    conj: "conj",
    disj: "disj"
}

var numbers = function (version, minjump, destructiveextent) {
    return " " + version + " " + minjump + " " + destructiveextent;
}

var uni = function (version, minjump, destructiveextent, k, graph) {
    var g1 = k(graph, version, minjump, destructiveextent);
    return g1.add_node(types.uni, "== " + numbers(version, minjump, destructiveextent));
};

var dot = function (version, minjump, destructiveextent, k, graph) {
    return graph.add_node(types.uni, "...");
};

var conj = function(left, right) {
    return function (topversion, topminjump, topdestructiveextent, k, graph) {
        var g1 = graph.add_node(types.conj, "* " + numbers(topversion, topminjump, topdestructiveextent));

        var called = false;
        var newk = function (graph, bottomversion, bottomminjump, bottomdestructiveextent) {
            if (called) {
                throw "error - called twice. bad graph description."
            }

            called = true;

            var version, minjump;
            if (bottomdestructiveextent > topversion) {
                version = bottomversion + 1;
                minjump = bottomversion;
            } else {
                version = bottomversion;
                minjump = bottomminjump;
            }

            var g2 = right(version, minjump, topdestructiveextent, k, graph);
            return g2.add_edge(g1, g2);
        }

        var g3 = left(topversion, topminjump, topdestructiveextent, newk, g1);

        return g3.add_edge(g1, g3).set_node(g1);
    };
};

var disj = function(left, right) {
    return function (version, minjump, destructiveextent, k, graph) {
        var g1 = left(version + 1, minjump, version + 1, k, graph);
        var g2 = right(version + 1, minjump, version + 1, k, g1);

        var g3 = g2.add_node(types.disj, "+ " + numbers(version, minjump, destructiveextent));

        return g3.add_edge(g3, g2).add_edge(g3, g1);
    };
};

var visualize = function(expr) {
    var execution = expr(0, 0, 0, function (graph) { return graph; }, make_graph([], []));

    var data = {nodes: execution.nodes, edges: execution.edges};

    var container = document.getElementById('container');

    var options = {
        hierarchicalLayout: {
            nodeSpacing: 50,
            direction: "UD",
            layout: "direction"
        },
        smoothCurves: false,
        groups: {
            conj:
                {
                color: { background: "orange", border: "black" }
            },
            disj:
                {
                color: { background: "yellow", border: "black" }
            },
            uni:
                {
                color: { border: "black" }
            }
        }
    };

    var network = new vis.Network(container, data, options)
}
