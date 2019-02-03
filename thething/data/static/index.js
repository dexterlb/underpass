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

var render_input = function(input) {
    return $('<div />')
            .addClass('input-box')
            .text('input breakdown goes here');
}

var render_parse = function(name, parse) {
    return $('<div />')
        .addClass('parse')
        .append($('<div />')
            .addClass('parse-name')
            .text(name))
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
    box = $('<div />')
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
    set_status('empty');
}
