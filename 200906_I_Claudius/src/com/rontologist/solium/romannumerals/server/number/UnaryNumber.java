package com.rontologist.solium.romannumerals.server.number;

import com.rontologist.solium.romannumerals.server.numeral.unary.UnaryNumeral;
import com.rontologist.solium.romannumerals.server.numeral.Numeral;

public class UnaryNumber extends AbstractNumber<UnaryNumeral> {

    public int getValue() {
        int value = 0;
        for (Numeral n : getNumerals()) {
            value += n.getValue();
        }
        return value;
    }
}