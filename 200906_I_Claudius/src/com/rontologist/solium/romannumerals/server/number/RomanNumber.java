package com.rontologist.solium.romannumerals.server.number;

import com.rontologist.solium.romannumerals.server.numeral.Numeral;
import com.rontologist.solium.romannumerals.server.numeral.roman.RomanNumeral;

import java.util.List;
import java.util.Collections;

public class RomanNumber extends AbstractNumber<RomanNumeral> {
    public RomanNumber() {
        this(Collections.<RomanNumeral>emptyList());
    }

    public RomanNumber(List<RomanNumeral> romanNumerals) {
        setNumerals(romanNumerals);
    }

    public int getValue() {
        int value = 0;
        for (Numeral n : getNumerals()) {
            value += n.getValue();
        }
        return value;
    }
}