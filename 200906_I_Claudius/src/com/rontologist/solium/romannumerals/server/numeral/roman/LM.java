package com.rontologist.solium.romannumerals.server.numeral.roman;

public class LM extends AbstractRomanNumeral {
    private static final LM INSTANCE = new LM();
    private static final int VALUE = 950;

    private LM() { }

    public static LM getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}