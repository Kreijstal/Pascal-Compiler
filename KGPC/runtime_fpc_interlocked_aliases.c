/* FPC alias wrappers for interlocked operations. */
long FPC_INTERLOCKEDEXCHANGEADD(long *target, long value)
{
    return __sync_fetch_and_add(target, value);
}

long long FPC_INTERLOCKEDCOMPAREEXCHANGE64(long long *target, long long new_val, long long comparand)
{
    return __sync_val_compare_and_swap(target, comparand, new_val);
}
