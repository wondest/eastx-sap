// @ts-ignore
/* eslint-disable */
declare namespace API {

  type ErrorResponse = {
    /** 业务约定的错误码 */
    errorCode: string;
    /** 业务上的错误信息 */
    errorMessage?: string;
    /** 业务上的请求是否成功 */
    success?: boolean;
  };

  type AsyncResponse = NoneReturn|ErrorResponse;
  
  type StockResponse = DataReturn|ErrorResponse;

  type NoneReturn = {
    /** 业务上的请求是否成功 */
    success?: boolean;
  };

  type DataReturn = {
    /** 列表的内容*/
    data: Stock;
    /** 业务上的请求是否成功 */
    success: boolean;
  }

  type Stock = {
    code: string;
    name?: string;
    exchange?: string;
    series: Bar[]
  }

  type Bar = {
    datetime: string;
    signal: Signal;
    price: Price;
    volume: number;
  };

  type Signal = {
    direction: "B"|"S"|"N";
    amount: number;
  }

  type Price = {
    open: number;
    high: number;
    low: number;
    close: number;
  }

  type MessageFrame = {
    command: "D"|"B"|"E";
    content?: string;
  }
}
  