package com.rontologist.solium.romannumerals.server.numeral;

public interface NumeralFactory<T extends Numeral> {
    public T getNumeral(int value);
}
