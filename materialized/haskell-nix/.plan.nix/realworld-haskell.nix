{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { ghcid = false; stan = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "realworld-haskell"; version = "0.3.0.0"; };
      license = "NONE";
      copyright = "Francis Chan (c) 2021-2022";
      maintainer = "jackychany321@gmail.com";
      author = "fanshi1028";
      homepage = "https://github.com/fanshi1028/realworld";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "backend/.postgres/migration/*.sql"
        "CHANGELOG.md"
        "golden/*.json"
        "README.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      sublibs = {
        "common-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
            (hsPkgs."password" or (errorHandler.buildDepError "password"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            ];
          buildable = true;
          modules = [
            "API"
            "API/Auth/User"
            "API/Authorization"
            "API/OptionalAuth"
            "API/Protected"
            "API/Public"
            "API/Util"
            "Data/Authentication/HasAuth"
            "Data/Authentication/Internal/HasAuth"
            "Data/Authentication/Internal/HasAuth/User"
            "Data/Domain"
            "Data/Domain/Article"
            "Data/Domain/Comment"
            "Data/Domain/Transform"
            "Data/Domain/User"
            "Data/Field/Bio"
            "Data/Field/Body"
            "Data/Field/Description"
            "Data/Field/Email"
            "Data/Field/Image"
            "Data/Field/Password"
            "Data/Field/Slug"
            "Data/Field/Tag"
            "Data/Field/Time"
            "Data/Field/Title"
            "Data/Field/Username"
            "Data/Paging"
            "Data/Storage/Error"
            "Data/Storage/Map"
            "Data/Storage/Map/Internal/HasCreate"
            "Data/Storage/Map/Internal/HasCreate/Article"
            "Data/Storage/Map/Internal/HasCreate/Comment"
            "Data/Storage/Map/Internal/HasCreate/User"
            "Data/Storage/Map/Internal/HasStorage"
            "Data/Storage/Map/Internal/HasStorage/Article"
            "Data/Storage/Map/Internal/HasStorage/Comment"
            "Data/Storage/Map/Internal/HasStorage/User"
            "Data/Storage/Map/Internal/HasUpdate"
            "Data/Storage/Map/Internal/HasUpdate/Article"
            "Data/Storage/Map/Internal/HasUpdate/User"
            "Data/Token/HasToken"
            "Data/Token/Internal/HasToken"
            "Data/Token/Internal/HasToken/User"
            "Data/Util/Impossible"
            "Data/Util/JSON/From"
            "Data/Util/JSON/To"
            "Data/Util/Sort"
            "Data/Util/Validation"
            "Effect/Authentication"
            "Effect/Cookie/Xsrf"
            "Effect/CreateSalt"
            "Effect/OptionalAuthAction"
            "Effect/OptionalAuthAction/Many"
            "Effect/Token/Create"
            "Effect/Token/Create/JWT"
            "Effect/Token/Decode"
            "Effect/Token/Decode/JWT"
            "Effect/Token/Invalidate"
            "Effect/Token/Invalidate/JWT"
            "Effect/UserAction"
            "Effect/UserAction/Many"
            "Effect/VisitorAction"
            ];
          hsSourceDirs = [ "common/src" ];
          };
        "backend-api-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            ] ++ (if flags.ghcid
            then [
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
              (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
              (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
              (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
              (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              ]
            else [
              (hsPkgs."realworld-haskell".components.sublibs.common-internal or (errorHandler.buildDepError "realworld-haskell:common-internal"))
              ]);
          buildable = true;
          modules = [
            "Server"
            "Server/Auth/User"
            "Server/OptionalAuth"
            "Server/Protected"
            "Server/Public"
            ];
          hsSourceDirs = [
            "backend/src/api"
            ] ++ (pkgs.lib).optional (flags.ghcid) "common/src";
          };
        "backend-in-mem-carrier-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."focus" or (errorHandler.buildDepError "focus"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            (hsPkgs."password" or (errorHandler.buildDepError "password"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            ] ++ (if flags.ghcid
            then [
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
              (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
              (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
              (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
              (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              ]
            else [
              (hsPkgs."realworld-haskell".components.sublibs.backend-api-internal or (errorHandler.buildDepError "realworld-haskell:backend-api-internal"))
              (hsPkgs."realworld-haskell".components.sublibs.common-internal or (errorHandler.buildDepError "realworld-haskell:common-internal"))
              ]);
          buildable = true;
          modules = [
            "InMem/App"
            "InMem/Authentication/User"
            "InMem/Authorization"
            "InMem/OptionalAuthAction"
            "InMem/OptionalAuthAction/Many"
            "InMem/Relation"
            "InMem/Relation/Internal/ManyToMany"
            "InMem/Relation/Internal/ManyToMany/Favorite"
            "InMem/Relation/Internal/ManyToMany/Follow"
            "InMem/Relation/Internal/ManyToMany/Tag"
            "InMem/Relation/Internal/ToMany"
            "InMem/Relation/Internal/ToMany/ArticleHasComment"
            "InMem/Relation/Internal/ToMany/UserCreateArticle"
            "InMem/Relation/Internal/ToMany/UserCreateComment"
            "InMem/Relation/Internal/ToOne"
            "InMem/Relation/Internal/ToOne/EmailOfUser"
            "InMem/Storage"
            "InMem/UserAction"
            "InMem/UserAction/Many"
            "InMem/VisitorAction"
            ];
          hsSourceDirs = [
            "backend/src/carrier/mem"
            ] ++ (pkgs.lib).optionals (flags.ghcid) [
            "common/src"
            "backend/src/api"
            ];
          };
        "backend-rel8-carrier-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."hasql-transaction" or (errorHandler.buildDepError "hasql-transaction"))
            (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
            (hsPkgs."password" or (errorHandler.buildDepError "password"))
            (hsPkgs."rel8" or (errorHandler.buildDepError "rel8"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            ] ++ (if flags.ghcid
            then [
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
              (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
              (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
              (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
              (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              ]
            else [
              (hsPkgs."realworld-haskell".components.sublibs.backend-api-internal or (errorHandler.buildDepError "realworld-haskell:backend-api-internal"))
              (hsPkgs."realworld-haskell".components.sublibs.common-internal or (errorHandler.buildDepError "realworld-haskell:common-internal"))
              ]);
          buildable = true;
          modules = [
            "InRel8/App"
            "InRel8/Authentication/User"
            "InRel8/OptionalAuthAction"
            "InRel8/OptionalAuthAction/Many"
            "InRel8/Sql"
            "InRel8/Storage"
            "InRel8/Storage/Internal/Field"
            "InRel8/Storage/Schema/Article"
            "InRel8/Storage/Schema/ArticleHasTag"
            "InRel8/Storage/Schema/Comment"
            "InRel8/Storage/Schema/Tag"
            "InRel8/Storage/Schema/User"
            "InRel8/Storage/Schema/UserFavoriteArticle"
            "InRel8/Storage/Schema/UserFollowUser"
            "InRel8/Storage/Schema/Util"
            "InRel8/UserAction"
            "InRel8/UserAction/Many"
            "InRel8/VisitorAction"
            ];
          hsSourceDirs = [
            "backend/src/carrier/rel8"
            ] ++ (pkgs.lib).optionals (flags.ghcid) [
            "common/src"
            "backend/src/api"
            ];
          };
        };
      exes = {
        "realworld-haskell-frontend" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."reflex" or (errorHandler.buildDepError "reflex"))
            (hsPkgs."reflex-vty" or (errorHandler.buildDepError "reflex-vty"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            ];
          buildable = true;
          hsSourceDirs = [ "frontend/app" ];
          mainPath = [ "Main.hs" ] ++ (pkgs.lib).optional (flags.stan) "";
          };
        "realworld-haskell-backend" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ] ++ (if flags.ghcid
            then [
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
              (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
              (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
              (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
              (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."focus" or (errorHandler.buildDepError "focus"))
              (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
              (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
              (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
              (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
              (hsPkgs."hasql-transaction" or (errorHandler.buildDepError "hasql-transaction"))
              (hsPkgs."rel8" or (errorHandler.buildDepError "rel8"))
              ]
            else [
              (hsPkgs."realworld-haskell".components.sublibs.backend-in-mem-carrier-internal or (errorHandler.buildDepError "realworld-haskell:backend-in-mem-carrier-internal"))
              (hsPkgs."realworld-haskell".components.sublibs.backend-rel8-carrier-internal or (errorHandler.buildDepError "realworld-haskell:backend-rel8-carrier-internal"))
              ]);
          buildable = true;
          hsSourceDirs = [
            "backend/app"
            ] ++ (pkgs.lib).optionals (flags.ghcid) [
            "common/src"
            "backend/src/api"
            "backend/src/carrier/mem"
            "backend/src/carrier/rel8"
            ];
          mainPath = ([ "Main.hs" ] ++ (pkgs.lib).optional (flags.stan) "") ++ [
            ""
            ];
          };
        };
      tests = {
        "realworld-haskell-backend-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."jose" or (errorHandler.buildDepError "jose"))
            (hsPkgs."password" or (errorHandler.buildDepError "password"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-streamly" or (errorHandler.buildDepError "servant-streamly"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."focus" or (errorHandler.buildDepError "focus"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."hasql-transaction" or (errorHandler.buildDepError "hasql-transaction"))
            (hsPkgs."rel8" or (errorHandler.buildDepError "rel8"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fakedata" or (errorHandler.buildDepError "fakedata"))
            (hsPkgs."fakedata-quickcheck" or (errorHandler.buildDepError "fakedata-quickcheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-golden-aeson" or (errorHandler.buildDepError "hspec-golden-aeson"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."postgresql-migration" or (errorHandler.buildDepError "postgresql-migration"))
            (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-arbitrary-adt" or (errorHandler.buildDepError "quickcheck-arbitrary-adt"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            (hsPkgs."tmp-postgres" or (errorHandler.buildDepError "tmp-postgres"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ] ++ (pkgs.lib).optionals (!flags.ghcid) [
            (hsPkgs."realworld-haskell".components.sublibs.backend-api-internal or (errorHandler.buildDepError "realworld-haskell:backend-api-internal"))
            (hsPkgs."realworld-haskell".components.sublibs.backend-in-mem-carrier-internal or (errorHandler.buildDepError "realworld-haskell:backend-in-mem-carrier-internal"))
            (hsPkgs."realworld-haskell".components.sublibs.backend-rel8-carrier-internal or (errorHandler.buildDepError "realworld-haskell:backend-rel8-carrier-internal"))
            (hsPkgs."realworld-haskell".components.sublibs.common-internal or (errorHandler.buildDepError "realworld-haskell:common-internal"))
            ];
          buildable = true;
          modules = [
            "Gen/Naive"
            "Gen/Realistic"
            "Orphans"
            "Roundtrip"
            "StateMachine"
            "StateMachine/Gen"
            "StateMachine/Types"
            "StateMachine/Util"
            ];
          hsSourceDirs = [ "test" ] ++ (pkgs.lib).optionals (!(!flags.ghcid)) [
            "common/src"
            "backend/src/api"
            "backend/src/carrier/mem"
            "backend/src/carrier/rel8"
            ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }