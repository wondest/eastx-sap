package com.eastx.sap.batch.batchTdx;

import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

@Slf4j
@Component
@StepScope
public class BarItemProcessor implements ItemProcessor<TradeBarRO, TradeBarDO> {
	/**
	 * 
	 *
	 */
	private final BigDecimal divisor = BigDecimal.valueOf(100);

	private static SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd");

	@Value("#{jobParameters['code']}")
	private String code;

	@Value("#{jobParameters['bizDate']}")
	private Date bizDate;

	@Override
	public TradeBarDO process(final TradeBarRO raw) throws ParseException {
		Date datetime = dateFormatter.parse(String.valueOf(raw.getDatetime()));

		//>= bizDate
		if(datetime.compareTo(bizDate) >= 0) {
			System.out.println(raw);
			TradeBarDO target = new TradeBarDO();
			target.setDatetime(String.valueOf(raw.getDatetime()));
			target.setOpen(decimalValue(raw.getOpen()));
			target.setHigh(decimalValue(raw.getHigh()));
			target.setLow(decimalValue(raw.getLow()));
			target.setClose(decimalValue(raw.getClose()));
			return target;
		} else {
			return null;
		}
	}

	private double decimalValue(int a) {
		return BigDecimal.valueOf(a).divide(divisor, RoundingMode.HALF_UP).doubleValue();
	}
}
