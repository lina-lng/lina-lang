-- Lina runtime helpers for coroutine support
local function _lina_coroutine_resume(co)
  local ok, result = coroutine.resume(co)
  if ok then
    return {_tag = 0, _0 = result}
  else
    return {_tag = 1, _0 = result}
  end
end

local function _lina_call_generator(gen)
  return gen()
end

-- Debug stack frame offset constant
-- When Debug.getlocal is called from user code, the call stack looks like:
--   [user level + 0] User code calling Debug.getlocal
--   [user level + 1] Curried wrapper (index argument applied)
--   [user level + 2] Curried wrapper (level argument applied)
--   [user level + 3] IIFE wrapper (module loading pattern)
--   [user level + 4] _lina_debug_getlocal (this helper function)
-- To reach the user's requested stack frame, we add 4 to skip internal frames.
local DEBUG_STACK_FRAME_OFFSET = 4

local function _lina_debug_getlocal(level, index)
  local name, value = debug.getlocal(level + DEBUG_STACK_FRAME_OFFSET, index)
  if name == nil then
    return nil
  else
    return {name = name, value = value}
  end
end

local function _lina_debug_getupvalue(func, index)
  local name, value = debug.getupvalue(func, index)
  if name == nil then
    return nil
  else
    return {name = name, value = value}
  end
end
