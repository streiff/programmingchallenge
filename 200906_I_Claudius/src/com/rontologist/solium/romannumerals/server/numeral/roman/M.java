package com.rontologist.solium.romannumerals.server.numeral.roman;

public class M extends AbstractRomanNumeral {
    private static final M INSTANCE = new M();
    private static final int VALUE = 1000;

    private M() { }

    public static M getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}