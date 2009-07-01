package com.rontologist.solium.romannumerals.server.number;

import com.rontologist.solium.romannumerals.server.numeral.unary.UnaryNumeral;
import com.rontologist.solium.romannumerals.server.numeral.Numeral;

import java.util.LinkedList;
import java.util.Collections;
import java.util.List;

public class UnaryNumber extends AbstractNumber<UnaryNumeral> {

    public static final UnaryNumber ONE = new UnaryNumber(
            Collections.singletonList(UnaryNumeral.getInstance()));

    public UnaryNumber(List<UnaryNumeral> numerals) {
        setNumerals(numerals);
   }                                         

    public int getValue() {
        return getNumerals().size();
    }

    public UnaryNumber subtract(UnaryNumber n) {
        return new UnaryNumber(
                new LinkedList<UnaryNumeral>(getNumerals().subList(
                        n.getValue(), getNumerals().size())
                )
        );
    }
}
