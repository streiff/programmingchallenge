package com.rontologist.solium.romannumerals.server.numeral.roman;

public class X extends AbstractRomanNumeral {
    private static final X INSTANCE = new X();
    private static final int VALUE = 10;

    private X() { }

    public static X getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}