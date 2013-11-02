Elm.Native.Text = {};
Elm.Native.Text.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Text = elm.Native.Text || {};
    if (elm.Native.Text.values) return elm.Native.Text.values;

    var JS = Elm.JavaScript.make(elm);
    var Utils = Elm.Native.Utils.make(elm);
    var Color = Elm.Native.Color.make(elm);
    var Element = Elm.Graphics.Element.make(elm);
    var show = Elm.Native.Show.make(elm).show;

    function makeSpaces(s) {
        if (s.length == 0) { return s; }
        var arr = s.split('');
        if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
        for (var i = arr.length; --i; ) {
            if (arr[i][0] == ' ' && arr[i-1] == ' ') {
                arr[i-1] = arr[i-1] + arr[i];
                arr[i] = '';
            }
        }
        for (var i = arr.length; i--; ) {
            if (arr[i].length > 1 && arr[i][0] == ' ') {
                var spaces = arr[i].split('');
                for (var j = spaces.length - 2; j >= 0; j -= 2) {
                    spaces[j] = '&nbsp;';
                }
                arr[i] = spaces.join('');
            }
        }
        arr = arr.join('');
        if (arr[arr.length-1] === " ") {
	    return arr.slice(0,-1) + '&nbsp;';
        }
        return arr;
    }

    function properEscape(str) {
        if (str.length == 0) return str;
        str = str //.replace(/&/g,  "&#38;")
	    .replace(/"/g,  '&#34;')
	    .replace(/'/g,  "&#39;")
	    .replace(/</g,  "&#60;")
	    .replace(/>/g,  "&#62;")
	    .replace(/\n/g, "<br/>");
        var arr = str.split('<br/>');
        for (var i = arr.length; i--; ) {
	    arr[i] = makeSpaces(arr[i]);
        }
        return arr.join('<br/>');
    }

    function toText(str) { return Utils.txt(properEscape(JS.fromString(str))); }

    function addTag(tag) {
        return function(text) {
            return Utils.txt('<' + tag + '>' + text + '</' + tag + '>');
        }
    }
    
    function addStyle(style, value, text) {
        return Utils.txt("<span style='" + style + ":" + value + "'>" + text + "</span>");
    }

    function typeface(name, text) {
        return addStyle('font-family', JS.fromString(name), text);
    }
    function monospace(text) {
        return addStyle('font-family', 'monospace', text);
    }
    function size(px, text) { return addStyle('font-size', px + 'px', text) }
    var header = addTag('h1');
    function height(h, text) { return addStyle('font-size', h+'px', text) }
    function italic(text) { return addStyle('font-style', 'italic', text) }
    var bold = addTag('b');

    function extract(c) {
        if (c._3 === 1) { return 'rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')'; }
        return 'rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')';
    }
    function color(c, text) {
        return addStyle('color', extract(c), text);
    }
    function underline(text) { return addStyle('text-decoration', 'underline', text) }
    function overline(text) { return addStyle('text-decoration', 'overline', text) }
    function strikeThrough(text) {
        return addStyle('text-decoration', 'line-through', text);
    }
    function link(href, text) {
        return Utils.txt("<a href='" + toText(href) + "'>" + text + "</a>");
    }

    function position(align) {
        return function(text) {
            var raw = {
                ctor:'RawHtml',
                html: '<span style="text-align:' + align + ';">' + text + '</span>',
                guid: null,
                args: [],
            };
            var pos = A2(Utils.htmlHeight, 0, raw);
            return A3(Element.newElement, pos._0, pos._1, raw);
        }
    }

    function markdown(text, guid) {
        var raw = {
            ctor:'RawHtml',
            html: text,
            guid: guid,
            args: Array.prototype.slice.call(arguments, 2).map(function(arg) {
                if (arg.props && arg.element) {
                    arg.isElement = true;
                    return arg;
                } else if (!arg.isText) {
                    return Utils.txt('<code>' + show(arg) + '</code>');
                }
                return arg;
            }),
        };
        var pos = A2(Utils.htmlHeight, 0, raw);
        return A3(Element.newElement, pos._0, pos._1, raw);
    }

    function asText(v) {
        return position('left')(monospace(toText(show(v))));
    }

    function plainText(v) {
        return position('left')(toText(v));
    }

    return elm.Native.Text.values = {
        toText: toText,

        header : header,
        height : F2(height),
        italic : italic,
        bold : bold,
        underline : underline,
        overline : overline,
        strikeThrough : strikeThrough,
        monospace : monospace,
        typeface : F2(typeface),
        color : F2(color),
        link : F2(link),

        justified : position('justify'),
        centered : position('center'),
        righted : position('right'),
        text : position('left'),
        plainText : plainText,
        markdown : markdown,

        asText : asText,
    };
};