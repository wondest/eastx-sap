/** Request 网络请求工具 更详细的 api 文档: https://github.com/umijs/umi-request/blob/master/README_zh-CN.md# **/
import { extend, ResponseError } from 'umi-request';
import { notification } from 'antd';

/**
 * @name errorHandler
 * @description 统一错误处理函数
 * @tutorial 应该是传入request.catch处理函数,只有在request中出现error才会进入到这里
 * @description 被动error比如503、404等；或者主动抛错
 * @param error 
 * @returns 
 */
const errorHandler = (error: ResponseError) => {
    console.debug("===== errorHandler ====>");
    if(error.response) {
        if(error.type === 'ServiceError') {
            //业务报错: 请求已发送但服务端返回状态码 2xx 的响应,但处理失败
            const errorTitle = `${error.type} - ${error.name}`
            const errorText = `${error.data.errorCode} ${error.data.errorMessage}`;

            notification.error({
                message: errorTitle,
                description: errorText,
            });
        } else {
            //请求已发送但服务端返回状态码非 2xx 的响应
            const method = error.request.options.method;
            const url = error.response.url;
            const status = error.response.status;
            const statusText = error.response.statusText;
            const errorTitle = `${error.type} - ${error.name}`
            const errorText = `${method} ${url} ${status} (${statusText})`

            notification.error({
                message: errorTitle,
                description: errorText,
            });
        }
    } else {
        //请求初始化时出错或者没有响应返回的异常
        const errorTitle = `${error.type} - ${error.name}`;
        const errorText = `远程服务未响应 (${error.message})`
        notification.error({
            message: errorTitle,
            description: errorText,
        });
    }

    console.debug("<==== errorHandler =====");
    throw error;
};

/** 实例化一个request **/
const request = extend({
    errorHandler, // 默认错误处理
});

/**
 * request - 1
 *    request interceptors
 *    middleware
 * 
 * await remote response - 2
 * 
 * response - 3
 *    response interceptors
 *    middleware
 */

/**
 * request 拦截器
 */
request.interceptors.request.use((url, options) => {
    console.debug(" === request interceptor === 1");
    return {
        url: `${url}`,
        options: { ...options },
    };
});


/**
 * response 拦截器
 */
request.interceptors.response.use(async (response, options) => {
    const data = await response.clone().json();

    console.debug(" === response interceptor === 1");

    //业务报错主动抛出异常，交给errorHandler处理
    //此处用ServiceError在errorHandler进行识别
    if (data.success === false) {
      let error = <ResponseError>{};

      error.message = data.errorMessage;
      error.data = data;
      error.name = 'ResponseFail';
      error.response = response;
      error.type = 'ServiceError';
    
      throw error;
    }
  
    return response;
});

/**
 * middleware 中间件
 */
request.use(async (context, next) => {

    console.debug(" === middle === 1");
    console.debug(context);

    await next();

    console.debug(" ==== middle === 2")
    console.debug(context);
});

export default request;