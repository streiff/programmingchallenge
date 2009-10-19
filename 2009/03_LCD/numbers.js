var TMPL = [[1, 3, 0, 3, 1], [0, 2, 0, 2, 0], [1, 2, 1, 1, 1], [1, 2, 1, 2, 1], [0, 3, 1, 2, 0], [1, 1, 1, 2, 1], 
            [1, 1, 1, 3, 1], [1, 2, 0, 2, 0], [1, 3, 1, 3, 1], [1, 3, 1, 2, 1], [0, 0, 1, 0, 0]
];

function $(divId) { return document.getElementById(divId); }

function renderNumber(num, sz, divId) {
    if (!_validate(num, sz, divId)) { return; }

    var digits = [];
    if (num < 0) { digits.push(_renderDigit(10, sz)); }
    for (i = _digitGen(num); ((d = i.next()) != undefined); digits.push(_renderDigit(d, sz)));

    var output = "";
    for (i = 0; i < (3 + sz * 2); ++i) {
        for (j = 0; j < digits.length; ++j) { output += digits[j][i] + " "; }
        output += "\n";
    }
    $(divId).innerHTML = output;
}

function renderNumberHollywood(num, sz, divId) {
    if (!_validate(num, sz, divId)) { return; }

    var digits = [];
    var rndDigits = [];
    for (var i = _digitGen(num); ((d = i.next()) != undefined);) { 
        digits.push(d); 
        rndDigits.push(parseInt(Math.random() * 10));
    }

    var tryNewNumber = function() {
        renderNumber((num < 0 ? "-" : "") + rndDigits.join(""), sz, divId);
        let numberReset = false;
        for (i = 0; i < rndDigits.length; ++i) {
            if (rndDigits[i] != digits[i]) {
                rndDigits[i] = parseInt(Math.random() * 10);
                numberReset = true;
            }
        }
        if (numberReset) { setTimeout(tryNewNumber, 250); }
    }
    tryNewNumber();
}

function _validate(num, sz, divId) {
    var errs = [];

    if (!num.match(/^-?[0-9]+$/)) { errs.push("Number an integer"); }
    if (!sz.match(/^[1-9][0-9]*$/)) { errs.push("Size not a positive integer"); }

    if (errs.length > 0) {
        $(divId).innerHTML = "";
        for (var i = 0; i < errs.length; ++i) { $(divId).innerHTML += errs[i] + "\n"; }
    }
    return errs.length == 0;
}

function _renderDigit(digit, sz) {
    var tmpl = TMPL[digit];
    var digitLines = [];

    for (var i = 0; i < tmpl.length; ++i) {
        if (i % 2 == 0) {
            let chr = tmpl[i] == 0 ? " " : "-";
            let str = " ";
            for (var j = 0; j < sz; ++j) { str += chr; }
            str += " ";
            digitLines.push(str);
        } else {
            for (var j = 0; j < sz; ++j) {
                let str = (tmpl[i] & 1) == 1 ? "|" : " ";
                for (var k = 0; k < sz; ++k) { str += " "; }
                str += (tmpl[i] & 2) == 2 ? "|" : " ";
                digitLines.push(str);
            }
        }
    }
    return digitLines;
}

function _digitGen(num) {
    if (num.charAt(0) == '-') { num = num.substring(1); }

    var digits = [];
    while (num.length > 0) {
        digits.push(num.substr(0, 1));
        num = num.substring(1);
    }

    for (let i = 0; i < digits.length; ++i) { yield digits[i]; }
    yield undefined;
}
