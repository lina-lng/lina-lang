(** Package management for Lina with LuaRocks compatibility.

    This module provides dependency management, including:
    - Semantic versioning and constraint parsing
    - LuaRocks package installation
    - Lockfile generation for reproducible builds
    - FFI binding generation for type safety

    @see {!Version} for version parsing and constraints
    @see {!Types} for shared type definitions *)

(** Re-exported modules *)

module Version = Version
module Types = Types
module Luarocks = Luarocks
module Lockfile = Lockfile
module Installer = Installer
module Binding_generator = Binding_generator
module Loader = Loader
