import { ResponseError } from 'umi-request';

/**
 * 
 * @param promise 
 * @returns [err, data]
 */
async function split<T>(promise : Promise<T>) : Promise<[ResponseError|null, T|null]> {
    try {
        const data = await promise;
        return [null, data];
    } catch (err) {
        return [err, null];
    }
}

export default split;