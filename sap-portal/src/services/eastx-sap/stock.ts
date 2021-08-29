import request from './infra/request';
import response from './infra/response'

/** 此处后端没有提供注释 GET /ws/stock */
export async function flow(options?: { [key: string]: any }) {
    return response(request<API.AsyncResponse>('/dev/api/stock/flow', {
      method: 'GET',
      ...(options || {}),
    }));
}

/** 此处后端没有提供注释 GET /ws/stock */
export async function once(options?: { [key: string]: any }) {
  return response(request<API.StockResponse>('/dev/api/stock/once', {
    method: 'GET',
    ...(options || {}),
  }));
}