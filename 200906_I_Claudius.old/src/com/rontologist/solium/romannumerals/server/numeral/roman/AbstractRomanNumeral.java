package com.rontologist.solium.romannumerals.server.numeral.roman;

public abstract class AbstractRomanNumeral implements RomanNumeral {

    public String getRepresentation() {
        return getClass().getSimpleName();
    }
}
