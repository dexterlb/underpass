var set_status = function(status) {
    var statuses = {
        empty:   false,
        error:   false,
        ok:      false,
        loading: false,
    };

    if (statuses[status] != false) {
        console.log("unknown status", status);
        return;
    }
    statuses[status] = true;

    for (var k in statuses) {
        if (statuses[k]) {
            $('#container').addClass("status-" + k);
            $('#container').removeClass("status-no-" + k);
        } else {
            $('#container').addClass("status-no-" + k);
            $('#container').removeClass("status-" + k);
        }
    }
}

var render_token = function(token) {
    var text = token.text;

    var tok = $('<span />')
        .addClass('token')
        .addClass('meta-container')
        .append($('<span />')
            .addClass('token-text')
            .text(text))
        .append(render_meta_box(token.tags));

    if (!text) {
        tok.addClass('fictional');
    }

    return tok;
}

var render_meta_box = function(meta) {
    var box = $('<div />').addClass('meta-box');
    for (var k in meta) {
        box.append($('<div />')
            .addClass('meta-tag')
            .text(k + ': ' + meta[k]));
    }
    return box;
}

var meta_container = function(el) {
    el.addClass('meta-container');
    el.hover(
        function() {
            var el = $(this);
            $('.meta-container-hovered').removeClass('meta-container-hovered');
            el.addClass('meta-container-hovered');
        },
        function() { $( this ).removeClass('meta-container-hovered'); },
    );
    return el;
}

var render_input = function(input) {
    var box = $('<div />')
        .addClass('input-box');

    for (var i = 0; i < input.length; i++) {
        if (i != 0) {
            box.append($('<span />').addClass('token-sep'));
        }
        box.append(render_token(input[i]));
    }

    return box;
}

var render_type = function(type) {
    return $('<span />').text('T');
}

var render_term = function(term) {
    var s = meta_container($('<span />').addClass('lambda-term'))
    switch(term._t) {
        case 'variable':
            return s
                .addClass('lambda-variable')
                .text(term.name)
                .append(render_meta_box({type: render_type(term.type), item: term._t, debruijn: term.index}));
        case 'constant':
            return s
                .addClass('lambda-constant')
                .text(term.name)
                .append(render_meta_box({type: render_type(term.type), item: term._t}));
        case 'application':
            s.addClass('lambda-application');
            for (var i = 0; i < term.terms.length; i++) {
                s.append(render_term(term.terms[i]))
            }
            return s
                .append(render_meta_box({type: render_type(term.type), item: term._t}));
        case 'lambda':
            return s
                .addClass('lambda-abstraction')
                .append($('<span />')
                    .addClass('lambda-abstraction-variable')
                    .append(meta_container($('<span />')
                        .addClass('lambda-abstraction-varname')
                        .text(term.varname))
                        .append(render_meta_box({type: term.vartype}))))
                .append(render_term(term.subterm).addClass('lambda-abstraction-subterm'))
                .append(render_meta_box({type: render_type(term.type), item: 'abstraction'}));
        case 'cast':
            return s
                .addClass('lambda-cast')
                .append(render_type(term.type).addClass('lambda-cast-type'))
                .append(render_term(term.subterm).addClass('lambda-cast-subterm'))
                .append(render_meta_box({type: render_type(term.type), item: term._t}));
        default:
            console.log('unknown term type', term._t);
            return undefined;
    }
}

var render_parse = function(name, parse) {
    return $('<div />')
        .addClass('parse')
        .append($('<div />')
            .addClass('parse-name')
            .text(name))
        .append($('<div />')
            .addClass('minipass-term')
            .append(render_term(parse.minipass_term)))
        .append($('<div />')
            .addClass('output-query')
            .append($('<pre />').text(parse.output_query))
            .append($('<a target="_blank" rel="noopener noreferrer" />')
                .text('see on map')
                .attr('href', 'https://overpass-turbo.eu/?Q=' + encodeURIComponent(parse.output_query))
            )
        );
}

var render_parses = function(parses) {
    var box = $('<div />')
        .addClass('parses-box');

    for (var i = 0; i < parses.length; i++) {
        box.append(render_parse('Parse ' + (i + 1), parses[i]));
    }

    return box;
}

var render_response = function(data) {
    return $('<div />')
        .addClass('result-box')
        .append(render_input(data.input))
        .append(render_parses(data.parses));
}

var main = function() {
    $('#input_query').bind('enter_key', function(e) {
        query = $('#input_query').val();
        $.ajax({
            url: 'q/' + encodeURIComponent(query),
            success: function(data) {
                set_status('ok');
                console.log('received response', data);
                $('#result').empty();
                $('#result').append(render_response(data));
            },
            error: function(o, err, e) {
                set_status('error');
                $('#error').text(error);
            },
        });
        set_status('loading');
    });

    $('#input_query').keyup(function(e){
        if(e.keyCode == 13)
        {
            $(this).trigger("enter_key");
        }
    });
    $('#input_query').focus();
    set_status('empty');
}
