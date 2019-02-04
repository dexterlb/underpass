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

var expander = function(el) {
    return el
        .addClass('expander')
        .prepend($('<span />')
            .addClass('expander-button')
            .click(function() {
                el.toggleClass('expander-active');
            })
            .hover(function() {
                el.addClass('expander-hovered');
            }, function() {
                el.removeClass('expander-hovered');
            })
        );
}

var render_token = function(token) {
    var text = token.text;

    var tok = meta_container($('<span />')
        .addClass('token')
        .append($('<span />')
            .addClass('token-text')
            .text(text))
        .append(render_meta_box(token.tags)));

    if (!text) {
        tok.addClass('fictional');
    }

    return tok;
}

var render_meta_value = function(val) {
    if (typeof val === 'object') {
        if (val.addClass === undefined) {
            console.log('weird object ', val);
        }
        return val;
    }
    return $('<span />').text(val);
}

var render_meta_box = function(meta) {
    var box = $('<div />').addClass('meta-box');
    for (var k in meta) {
        box.append($('<div />')
            .addClass('meta-tag')
            .append($('<span />').addClass('meta-key').text(k))
            .append(render_meta_value(meta[k]).addClass('meta-value')));
    }
    return box;
}

var meta_container = function(el) {
    return el
        .addClass('meta-container')
        .prepend($('<span />')
            .addClass('meta-container-button')
            .click(function() {
                el.toggleClass('meta-container-active');
            })
            .hover(function() {
                el.addClass('meta-container-hovered');
            }, function() {
                el.removeClass('meta-container-hovered');
            })
        );
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

var basic_type_name = function(type) {
    if (!(typeof type === 'object')) {
        return type;
    }

    switch(type._t) {
        case 'subtype':
            return type.name;
        case 'basic_type':
            return type.type;
    }
}

var basic_type_meta = function(type) {
    if (!(typeof type === 'object')) {
        return {item: 'basic type', name: type};
    }

    switch(type._t) {
        case 'subtype':
            return {item: 'derived type', parent: render_type(type.parent), name: type.name};
        case 'basic_type':
            return {item: 'basic type', name: type.type}
    }
}

var render_type = function(type) {
    var s = $('<span />').addClass('lambda-type');
    switch(type._t) {
        case 'application':
            return s
                .addClass('lambda-type-application')
                .append(render_type(type.left))
                .append(render_type(type.left))
        case 'wildcard':
            return s
                .addClass('lambda-type-wildcard')
        case 'basic':
            return meta_container(s
                .addClass('lambda-type-basic')
                .text(basic_type_name(type.type))
                .append(render_meta_box(basic_type_meta(type.type))));
    }
}

var render_term = function(term) {
    var s = $('<span />').addClass('lambda-term');
    return meta_container(function() {
        switch(term._t) {
            case 'variable':
                return s
                    .addClass('lambda-variable')
                    .text(term.name)
                    .append(render_meta_box({type: render_type(term.type), name: term.name, item: term._t, debruijn: term.index}));
            case 'constant':
                return s
                    .addClass('lambda-constant')
                    .text(term.name)
                    .append(render_meta_box({type: render_type(term.type), name: term.name, item: term._t}));
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
                            .append(render_meta_box({type: render_type(term.vartype)}))))
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
    }());
}

var render_parse = function(name, parse) {
    return expander($('<div />'))
        .addClass('parse')
        .append($('<div />')
            .addClass('parse-name')
            .addClass('expander-header')
            .text(name))
        .append($('<div />')
            .addClass('expandable')
            .append(expander($('<div />'))
                .addClass('result-term')
                .append($('<div />')
                    .addClass('expander-header')
                    .text('Resulting lambda term'))
                .append($('<div />')
                    .append(render_term(parse.result_term))
                    .addClass('expandable')))
            .append(expander($('<div />'))
                .addClass('detyped-term')
                .append($('<div />')
                    .addClass('expander-header')
                    .text('Type-stripped term'))
                .append($('<div />')
                    .append(render_term(parse.detyped_term))
                    .addClass('expandable')))
            .append(expander($('<div />'))
                .addClass('reduced-term')
                .append($('<div />')
                    .addClass('expander-header')
                    .text('Reduced term'))
                .append($('<div />')
                    .append(render_term(parse.reduced_term))
                    .addClass('expandable')))
            .append(expander($('<div />'))
                .addClass('output-query')
                .addClass('expander-active')
                .append($('<div />')
                    .addClass('expander-header')
                    .text('Overpass query'))
                .append($('<div />')
                    .addClass('expandable')
                    .append($('<pre />').text(parse.output_query))
                    .append($('<a target="_blank" rel="noopener noreferrer" />')
                        .text('see on map')
                        .attr('href', 'https://overpass-turbo.eu/?Q=' + encodeURIComponent(parse.output_query))
                    )
                )
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
