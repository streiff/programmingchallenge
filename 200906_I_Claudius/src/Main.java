import com.rontologist.solium.romannumerals.server.number.Number;
import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;
import com.rontologist.solium.romannumerals.server.numeral.unary.UnaryNumeral;
import com.rontologist.solium.romannumerals.server.converter.NumberConverterFactory;
import com.rontologist.solium.romannumerals.server.converter.NumberConverter;

import java.util.Arrays;

public class Main {

    public static void main(String[] args) {
        UnaryNumeral[] numerals = new UnaryNumeral[1888];
        Arrays.fill(numerals, UnaryNumeral.getInstance());
        UnaryNumber number = new UnaryNumber(Arrays.asList(numerals));

        NumberConverterFactory numberConverterFactory = NumberConverterFactory.getInstance();
        NumberConverter<UnaryNumber, RomanNumber> converter = numberConverterFactory.getConverter(UnaryNumber.class, RomanNumber.class);
        Number number2 = converter.convert(number);

        System.out.println(number2.getRepresentation());

    }
}
